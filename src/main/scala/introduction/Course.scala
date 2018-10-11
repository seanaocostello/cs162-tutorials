
package edu.ucsb.cs.cs162.tuts.introduction

// A course, with its subject and number
class Course(subject: String, number: Integer) {
  require(number > UniversityStandard.minimalCourseNumber)
  require(number < UniversityStandard.maximalCourseNumber)
  require(subject.length >= UniversityStandard.minimalSubjectLength)
  require(subject.length <= UniversityStandard.maximalSubjectLength)

  // The name of the course is the concatenation of subject 
  //  and number with no space in-between
  // Example: if subject is ANT and number is 101, the name is ANT101
  def name: String = subject + number

  // A lower division course is one with a number between 1 and 99.
  def isLowerDivisionCourse: Boolean = number > 0 && number < 100

  // An upper division course is one with a number between 100 and 199.
  def isUpperDivisionCourse: Boolean = number <= 199 && number >= 100

  // A graduate division course is one with a number between 200 and 299.
  def isGradDivisionCourse: Boolean = number <= 299 && number >= 200

  // A seminar is a course with a number between 500 and 599.
  def isSeminar: Boolean = number <= 599 && number >= 500
}
