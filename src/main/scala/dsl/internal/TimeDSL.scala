package dsl.internal

case class Time(hours: Int = 0, minutes: Int = 0) {
  require(hours >= 0 && hours <= 23, "hours must be within 0 and 23")
  require(minutes >= 0 && minutes <= 59, "minutes must be within 0 and 59")
}

object TimeDSL {
  
  // Exercise 1:
  // Make the following syntaxes work.
  // Hint: All we need are operator notation and implicit conversions
  // val t1: Time = 11 am
  // val t2: Time = 11 :: 30
  // val t3: Time = 11 :: 30 pm
    
}

