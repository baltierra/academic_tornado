#-----------------------------
#  WARMUP EXERCISES - PART 2 
#                           
#  FABIÁN ARANEDA-BALTIERRA  
#  CAPP 30122 - 02/12/2022
#-----------------------------

import sqlite3

connection = sqlite3.connect("course_information.sqlite3")
c = connection.cursor()


# 1. Find the titles of all courses with department code "CMSC" in the course
#    table.

dept = "CMSC"
query_1 = '''SELECT * FROM courses WHERE dept= ?'''
result_1 = c.execute(query_1, (dept,)).fetchall()

for item in result_1:
    print(item)

input("\nPress a key to continue...\n")



# 2. Find the department names, course numbers, and section numbers for courses
#    being offered on MWF at 10:30am (represented as 1030).

start_t = 1030; day = "MWF"
query_2 = '''
SELECT courses.dept, courses.course_num, sections.section_num
    FROM courses, meeting_patterns JOIN sections
        ON meeting_patterns.meeting_pattern_id = sections.meeting_pattern_id
    WHERE meeting_patterns.time_start = ?
        AND meeting_patterns.day = ?
        AND sections.course_id = courses.course_id
'''
result_2 = c.execute(query_2, (start_t, day)).fetchall()

for item in result_2:
    print(item)

input("\nPress a key to continue...\n")


    
# 3. Find the department names and course numbers for courses being offered in
# Ryerson on MWF between 10:30am and 3pm (represented as 1500).
time_1 = 1030; time_2 = 1500; day = "MWF"; building  = "RY"
query_3 = '''
SELECT courses.dept, courses.course_num
    FROM courses, meeting_patterns JOIN sections
        ON meeting_patterns.meeting_pattern_id = sections.meeting_pattern_id
    WHERE meeting_patterns.time_start >= ?
        AND meeting_patterns.time_start <= ?
        AND meeting_patterns.day = ?
        AND sections.building_code = ?
        AND sections.course_id = courses.course_id
'''
result_3 = c.execute(query_3, (time_1, time_2, day, building)).fetchall()

for item in result_3:
    print(item)

input("\nPress a key to continue...\n")



# 4. Find the department names, course numbers, and course titles for courses
#    being offered on MWF at 9:30am (represented as 930) that have the words
#    “programming” and “abstraction” in their title/course description.

start_t = 930; day = "MWF"; sub_1 = "programming"; sub_2 =  "abstraction"
query_4 = '''
SELECT courses.dept, courses.course_num, courses.title
    FROM courses, catalog_index, meeting_patterns JOIN sections
        ON meeting_patterns.meeting_pattern_id = sections.meeting_pattern_id
    WHERE sections.course_id = courses.course_id
        AND sections.course_id = catalog_index.course_id
        AND meeting_patterns.time_start = ?
        AND meeting_patterns.day = ?
        AND catalog_index.word IN (?, ?)
'''
result_4 = c.execute(query_4, (start_t, day, sub_1, sub_2)).fetchall()

for item in result_4:
    print(item)


connection.close()
