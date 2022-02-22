"""
CAPP 30122: Course Search Engine Part 1

Fabián A. Araneda-Baltierra
"""
# DO NOT REMOVE THESE LINES OF CODE
# pylint: disable-msg=invalid-name, redefined-outer-name, unused-argument, unused-variable

import json
import sys
import csv
import re
import bs4
import util
from util import convert_if_relative_url as ciru
from collections import deque #instead of "Queue" to have iterable queues

INDEX_IGNORE = set(['a', 'also', 'an', 'and', 'are', 'as', 'at', 'be','but',
                    'by', 'course', 'for', 'from', 'how', 'i', 'ii', 'iii',
                    'in', 'include', 'is', 'not', 'of', 'on', 'or', 's',
                    'sequence', 'so', 'social', 'students', 'such', 'that',
                    'the', 'their', 'this', 'through', 'to', 'topics', 'units',
                    'we', 'were', 'which', 'will', 'with', 'yet'])


def parse_course_info(tag_info):
    """
    Function that receives a BeautifulSoup object and returns a list with
    parsed information based on problem definition's criteria.

    Input: (bs4 object) html data.
    Output: (lst) list of strings with parsed information.
    """

    for tag in tag_info.find_all("p", class_="courseblocktitle"):
        course_data = tag.text.split(".")[0:2]
        course_data[0] = course_data[0].replace("\xa0", " ")
        course_data[1] = course_data[1].split(" ")[2:]
        aux = []
        aux.extend(course_data[0].lower().split(" "))

        for item in course_data[1]:
            word = re.sub('[,.!:]', '', item.lower())
            if word not in INDEX_IGNORE and word != '':
                aux.append(word)

        course_data[1] = aux

    for tag in tag_info.find_all("p", class_="courseblockdesc"):
        aux = tag.text.split(" ")
        for item in aux:
            word = re.sub('[,.!:\n\xa0]', '', item.lower())
            if word not in INDEX_IGNORE and word != '':# and word != '\n':
                course_data[1].append(word)

    return course_data


def JSON_to_Dict(json_file):
    """
    Function that receives a json file and return a dictionary from it.

    Input: (json) File.
    Output: (dict) Dictionary made from json file's information.
    """

    with open(json_file) as json_file:
        data = json.load(json_file)
        json_file.close()

    return data


def get_data_from_url(url):
    """
    Function that takes an url and returns formated data from it.
    """
    #create the soup
    r = util.get_request(url)
    r_data = util.read_request(r)
    soup = bs4.BeautifulSoup(r_data, "html5lib")
    url_data = []

    #look for courses info
    for div_tag in soup.find_all("div", class_="courseblock main"):
        t_subsequence = util.find_sequence(div_tag)
        if t_subsequence:

            main_desc =[]

            for tag in div_tag.find_all("p", class_="courseblockdesc"):
                main_text = tag.text.split(" ")

                for item in main_text:
                    word = re.sub('[,.!:\n\xa0]', '', item.lower())
                    if word not in INDEX_IGNORE and word != '':
                        main_desc.append(word)

            for p_tag in t_subsequence:
                whole_desc = parse_course_info(p_tag)
                whole_desc.append(main_text)
                url_data.append(whole_desc)

        else:
            url_data.append(parse_course_info(div_tag))

    return url_data

def index_data(num_pages_to_crawl, starting_url, limiting_domain):
    """
    Returns a list with url crawled content, which later will be used to build
    web page's index using mapping from a .json file.

    Inputs: (int) num_pages_to_crawl: maximum amount of pages to list.
            (str) starting_url: original URL to start crawling.
            (str) limiting_domain: domain space where all the valid URLs have to
                  belong.
    Output: (lst) List that contains the built index.
    """

    qToCrawl = deque([starting_url]) #queue to store urls to visit
    qProcessed = set() #set to keep track of visited urls and not visit twice
    data_for_csv = [] #list to record all course related data of our interest

    #start in depth search for all the urls that will need crawling
    while len(qToCrawl) and len(qProcessed) <= num_pages_to_crawl:
        url = qToCrawl.popleft() #take the first element of the queue
        r = util.get_request(url) #create a request with that element

        if r : #in case request is successful
            true_url = util.get_request_url(r) #get the true url

            #set conditions to store the right url
            if true_url not in qProcessed:
                qProcessed.add(true_url)
                #create the soup
                r_data = util.read_request(r)
                soup = bs4.BeautifulSoup(r_data, "html5lib")

                #add proper urls to the to-crawl-list and crawl the data
                for link in soup.find_all("a"):
                    #make sure there is an actual url into the <a> tag
                    if link.get("href"):
                        ok_url = ciru(true_url, link.get("href"))
                        if util.is_url_ok_to_follow(ok_url, limiting_domain):
                            if not (ok_url in qToCrawl or ok_url in qProcessed):
                                qToCrawl.append(ok_url)
                                data_for_csv.extend(get_data_from_url(ok_url))

    return data_for_csv


def create_csv_file(course_map_filename, index_filename, data_to_map):
    """
    Create a csv file maping a json file with identifiers for courses with the
    crawled data for each course.

    Inputs:
        course_map_filename: (str) Name of the .json file
        index_filename: (str) Name for the .csv file for the built index.
        data_to_map: (lst) List with all the relevant info crawled from URLs.
    Output:
        (.csv): File that contains the index for the crawled URLs.
        (bool): True, if files is built successfuly.
    """
    #create a dictionary with the content of course_map_filename
    course_map = JSON_to_Dict(course_map_filename)

    #maps course_map dict's content with data_to_map list to create the index
    with open(index_filename, 'w') as csv_file:
        for item in data_to_map:
            if item[0] in course_map.keys():
                for word in item[1]:
                    writer = csv.writer(csv_file, delimiter = "|")
                    writer.writerow([course_map[item[0]], word])

        csv_file.close()

        if csv_file.closed:
            return True


def go(num_pages_to_crawl, course_map_filename, index_filename):
    '''
    Crawl the college catalog and generates a CSV file with an index.

    Inputs:
        num_pages_to_crawl: the number of pages to process during the crawl
        course_map_filename: the name of a JSON file that contains the mapping
          course codes to course identifiers
        index_filename: the name for the CSV of the index.

    Outputs:
        CSV file of the index index.
    '''
    starting_url = ("http://www.classes.cs.uchicago.edu/archive/2015/winter"
                    "/12200-1/new.collegecatalog.uchicago.edu/index.html")
    limiting_domain = "classes.cs.uchicago.edu"

    #crawl all the urls to get the information to build the index
    data_to_map = index_data(num_pages_to_crawl, starting_url, limiting_domain)

    #create the csv index file
    if create_csv_file(course_map_filename, index_filename, data_to_map):
        return f"SUCCESS: Index file [{index_filename}] was correctly built."
    else:
        return f"ERROR: Index file [{index_filename}] couldn't be built."


if __name__ == "__main__":
    usage = "python3 crawl.py <number of pages to crawl>"
    args_len = len(sys.argv)
    course_map_filename = "course_map.json"
    index_filename = "catalog_index.csv"
    if args_len == 1:
        num_pages_to_crawl = 1000
    elif args_len == 2:
        try:
            num_pages_to_crawl = int(sys.argv[1])
        except ValueError:
            print(usage)
            sys.exit(0)
    else:
        print(usage)
        sys.exit(0)

    go(num_pages_to_crawl, course_map_filename, index_filename)
