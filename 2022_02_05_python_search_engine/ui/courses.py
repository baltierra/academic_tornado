"""
Course Search Engine: SEARCH

AUTHOR:
Fabián A. Araneda-Baltierra
"""

from math import radians, cos, sin, asin, sqrt, ceil
import sqlite3
import os

DATA_DIR = os.path.dirname(__file__)
DATABASE_FILENAME = os.path.join(DATA_DIR, "course_information.sqlite3")

REF_TABLE = {"terms": "catalog_index", "dept": "courses",
             "course_num": "courses", "title": "courses",
             "day": "meeting_patterns", "time_start": "meeting_patterns",
             "time_end": "meeting_patterns", "section_num": "sections",
             "enrollment": "sections", "building_code": "sections",
             "walking_time": "gps"}
TABLE_OUTPUT = {"courses": ["dept", "course_num", "title"],
                "meeting_patterns": ["day", "time_start", "time_end"],
                "sections": ["section_num", "enrollment"]}


def build_path(d):
    """
    The purpose of this function is to take the user input (dictionary) and
    compute the string containing three parts of the desired SQL queries (we
    call this the "path"):
        - SELECT
        - FROM
        - ON

    Input: d (a dictionary)

    Output: a string (a concatenation of SELECT, FROM, and ON for the query)
    """
    tables = set()
    tables.add("courses")
    for arg in d:
        tables.add(REF_TABLE[arg])

    if "meeting_patterns" in tables:
        tables.add("sections")
    if "sections" in tables:
        tables.add("meeting_patterns")

    select_command = (['.'.join(['courses', arg])
                      for arg in TABLE_OUTPUT["courses"]])
    from_command = ' FROM ' + ' JOIN '.join(tables)
    on_command = []
    if "catalog_index" in tables:
        on_command.append('courses.course_id = catalog_index.course_id')
    if "sections" in tables:
        select_command.append('sections.section_num')
        on_command.append('courses.course_id = sections.course_id')
        if "meeting_patterns" in tables:
            (on_command.append('sections.meeting_pattern_id \
                               = meeting_patterns.meeting_pattern_id'))
            select_command.extend(['.'.join(['meeting_patterns', arg]) \
                for arg in TABLE_OUTPUT["meeting_patterns"]])
        select_command.append('sections.enrollment')
    if "gps" in tables:
        select_command.append(' gps.building_code, time_between(gps.lon, \
                              gps.lat, origin.lon, origin.lat) AS \
                              walking_time ')
        from_command += ' JOIN (SELECT lon, lat, building_code FROM gps \
                        WHERE building_code = ?) AS origin'
        on_command.append('gps.building_code = sections.building_code')

    select_command = 'SELECT DISTINCT ' + ', '.join(select_command)
    if len(tables) > 1:
        on_command = ' ON ' + ' AND '.join(on_command)
        return select_command + from_command + on_command
    else:
        return select_command + from_command


def build_filter(d):
    """
    The purpose of this function is to take the user input (dictionary) and
    compute the string containing the WHERE part of the desired SQL queries
    (we call this the "filter"):
        - WHERE

    Input: d (a dictionary)

    Output: a string (a WHERE clause with the required filters for the query)
    """
    time_lb, time_ub = 0, 2359
    where_lst = []
    if "dept" in d:
        where_lst.append('courses.dept = "{}"'.format(d["dept"]))
    if "day" in d:
        day_command = ('meeting_patterns.day ' +
                       'IN ("{}")').format('", "'.join(d["day"]))
        where_lst.append(day_command)
    if "enrollment" in d:
        enroll_lb = d["enrollment"][0]
        enroll_ub = d["enrollment"][1]
        enroll_command = ('sections.enrollment BETWEEN ' +
                          '{} AND {}').format(enroll_lb, enroll_ub)
        where_lst.append(enroll_command)
    if "time_start" in d:
        time_lb = d["time_start"]
    if "time_end" in d:
        time_ub = d["time_end"]
    if "time_start" in d or "time_end" in d:
        time_command = ('meeting_patterns.time_start >= {} AND ' +
                        'meeting_patterns.time_end <= {}').format(time_lb, \
                                                                  time_ub)
        where_lst.append(time_command)
    if "walking_time" in d:
        gps_command = ' walking_time <= ?'
        where_lst.append(gps_command)
    if "terms" in d:
        term_filter = add_term_filter(d["terms"])
        where_lst.append(term_filter)

    where_command = ' WHERE ' + ' AND '.join(where_lst)
    return where_command


def add_term_filter(terms):
    """
    The purpose of this function is to take the list of terms input by the
    user and use it to append to our WHERE filter a nested query that takes
    all the course_ids mapped to those terms and groups them by those same
    course_ids. It will then only return those that appear the same number
    of times as the count of terms in the input terms list.

    Input: terms (a list of words/values the user is searching for)

    Output: a string (an addition of a nested query for our WHERE filter)
    """
    count = len(terms)
    words = '", "'.join(terms)
    terms_query = (' courses.course_id IN (SELECT course_id FROM ' +
                   'catalog_index WHERE word IN ("{}") GROUP BY course_id ' +
                   'HAVING COUNT(*)={});').format(words, count)
    return terms_query


def find_courses(args_from_ui):
    """
    Takes a dictionary containing search criteria and returns courses
    that match the criteria.  The dictionary will contain some of the
    following fields:
      - dept a string
      - day is list of strings
           -> ["'MWF'", "'TR'", etc.]
      - time_start is an integer in the range 0-2359
      - time_end is an integer an integer in the range 0-2359
      - enrollment is a pair of integers
      - walking_time is an integer
      - building_code ia string
      - terms is a list of strings string: ["quantum", "plato"]

    Returns a pair: an ordered list of attribute names and a list the
     containing query results.  Returns ([], []) when the dictionary
     is empty.
    """
    assert_valid_input(args_from_ui)

    if args_from_ui:
        path = build_path(args_from_ui)
        where_command = build_filter(args_from_ui)
        query = path + where_command
        info = sqlite3.connect(DATABASE_FILENAME)
        c = info.cursor()
        if "walking_time" in args_from_ui:
            info.create_function("time_between", 4, compute_time_between)
            data = c.execute(query, [args_from_ui["building_code"], \
                                     args_from_ui["walking_time"]])
        else:
            data = c.execute(query)
        header = get_header(data)
        body = data.fetchall()
        return header, body
    else:
        return ([], [])


########### auxiliary functions #################
def assert_valid_input(args_from_ui):
    """
    Verify that the input conforms to the standards set in the
    assignment.
    """
    assert isinstance(args_from_ui, dict)

    acceptable_keys = set(['time_start', 'time_end', 'enrollment', 'dept',
                           'terms', 'day', 'building_code', 'walking_time'])
    assert set(args_from_ui.keys()).issubset(acceptable_keys)

    # get both buiding_code and walking_time or neither
    has_building = ("building_code" in args_from_ui and
                    "walking_time" in args_from_ui)
    does_not_have_building = ("building_code" not in args_from_ui and
                              "walking_time" not in args_from_ui)

    assert has_building or does_not_have_building

    assert isinstance(args_from_ui.get("building_code", ""), str)
    assert isinstance(args_from_ui.get("walking_time", 0), int)

    # day is a list of strings, if it exists
    assert isinstance(args_from_ui.get("day", []), (list, tuple))
    assert all([isinstance(s, str) for s in args_from_ui.get("day", [])])

    assert isinstance(args_from_ui.get("dept", ""), str)

    # terms is a non-empty list of strings, if it exists
    terms = args_from_ui.get("terms", [""])
    assert terms
    assert isinstance(terms, (list, tuple))
    assert all([isinstance(s, str) for s in terms])

    assert isinstance(args_from_ui.get("time_start", 0), int)
    assert args_from_ui.get("time_start", 0) >= 0

    assert isinstance(args_from_ui.get("time_end", 0), int)
    assert args_from_ui.get("time_end", 0) < 2400

    # enrollment is a pair of integers, if it exists
    enrollment_val = args_from_ui.get("enrollment", [0, 0])
    assert isinstance(enrollment_val, (list, tuple))
    assert len(enrollment_val) == 2
    assert all([isinstance(i, int) for i in enrollment_val])
    assert enrollment_val[0] <= enrollment_val[1]


def compute_time_between(lon1, lat1, lon2, lat2):
    """
    Converts the output of the haversine formula to walking time in minutes
    """
    meters = haversine(lon1, lat1, lon2, lat2)

    # adjusted downwards to account for manhattan distance
    walk_speed_m_per_sec = 1.1
    mins = meters / (walk_speed_m_per_sec * 60)

    return int(ceil(mins))


def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])

    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * asin(sqrt(a))

    # 6367 km is the radius of the Earth
    km = 6367 * c
    m = km * 1000
    return m


def get_header(cursor):
    """
    Given a cursor object, returns the appropriate header (column names)
    """
    header = []

    for i in cursor.description:
        s = i[0]
        if "." in s:
            s = s[s.find(".")+1:]
        header.append(s)

    return header
