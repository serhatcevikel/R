printmessage <- function (a) {

                      if (is.na (a))

                                                       print ("a is a missing value!")

                     else if (a < 0)

                                                      print ("a is less than zero")

                                          else

                                                                          print ("a is greater than or equal to zero")

                    invisible (a)

                     }

printmessage (NA)
