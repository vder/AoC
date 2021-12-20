package y2021.day19

import cats.kernel.Monoid
import cats.kernel.Semigroup
import scala.annotation.tailrec
import cats.parse.Parser
import cats.parse.Rfc5234.*
import cats.implicits.*

object model {



val orient = 
List(
Vector( (0,1),(1,1),(2,1)),
Vector( (2,1),(1,1),(0,-1)),
Vector( (2,-1),(1,1),(0,1)),
Vector( (0,-1),(1,1),(2,-1)),

Vector ((2,1),(0,1),(1,1)),
Vector ((1,-1),(0,1),(2,1)),
Vector ((2,-1),(0,1),(1,-1)),
Vector ((1,1),(0,1),(2,-1)),

Vector ((1,1),(2,1),(0,1)),
Vector ((0,-1),(2,1),(1,1)),
Vector ((1,-1),(2,1),(0,-1)),
Vector ((0,1),(2,1),(1,-1)),

Vector ((0,1),(1,-1),(2,-1)),
Vector ((2,1),(1,-1),(0,1)),
Vector ((0,-1),(1,-1),(2,1)),
Vector ((2,-1),(1,-1),(0,1)),

Vector ((1,1),(0,-1),(2,1)),
Vector ((2,-1),(0,-1),(1,1)),
Vector ((1,-1),(0,-1),(2,-1)),
Vector ((2,1),(0,-1),(1,-1)),

Vector ((1,1),(2,-1),(0,-1)),
Vector ((0,1),(2,-1),(1,1)),
Vector ((1,-1),(2,-1),(0,1)),
Vector ((0,-1),(2,-1),(1,-1)))
 
}
