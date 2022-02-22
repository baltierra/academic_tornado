--------------------------------
-- WARMUP EXERCISES - PART 1 
--                           
-- FABIÁN ARANEDA-BALTIERRA  
-- CAPP 30122 - 02/12/2022
--------------------------------


-- 1. Find the titles of all courses with department code “CMSC” in the course
--    table
SELECT * FROM courses WHERE dept="CMSC";


-- 2. Find the department names, course numbers, and section numbers for
--    courses being offered on MWF at 10:30am (represented as 1030)
SELECT courses.dept, courses.course_num, sections.section_num
    FROM courses, meeting_patterns JOIN sections
        ON meeting_patterns.meeting_pattern_id = sections.meeting_pattern_id
    WHERE meeting_patterns.time_start = 1030 AND meeting_patterns.day = "MWF" AND sections.course_id = courses.course_id;
    

-- 3. Find the department names and course numbers for courses being offered in
--    Ryerson on MWF between 10:30am and 3pm (represented as 1500)
SELECT courses.dept, courses.course_num
    FROM courses, meeting_patterns JOIN sections
        ON meeting_patterns.meeting_pattern_id = sections.meeting_pattern_id
    WHERE meeting_patterns.time_start >= 1030
        AND meeting_patterns.time_start <= 1500
        AND meeting_patterns.day = "MWF"
        AND sections.building_code = "RY"
        AND sections.course_id = courses.course_id;


-- 4. Find the department names, course numbers, and course titles for courses
--    being offered on MWF at 9:30am (represented as 930) that have the words
--    “programming” and “abstraction” in their title/course description.
SELECT courses.dept, courses.course_num, courses.title
    FROM courses, catalog_index, meeting_patterns JOIN sections
        ON meeting_patterns.meeting_pattern_id = sections.meeting_pattern_id
    WHERE sections.course_id = courses.course_id
        AND sections.course_id = catalog_index.course_id
        AND meeting_patterns.time_start = 930
        AND meeting_patterns.day = "MWF"
        AND catalog_index.word IN ("programming", "abstraction");
