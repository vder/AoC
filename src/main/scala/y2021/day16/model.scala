package y2021.day16

object model {

  case class Version(value: Int) extends AnyVal
  case class TypeId(value: Int) extends AnyVal
//   case class A(value: Int) extends AnyVal
//   case class B(value: Int) extends AnyVal
//   case class C(value: Int) extends AnyVal
  case class LengthTypeId(value: Boolean) extends AnyVal

  sealed trait Package
  case class Literal(version: Version, typeId: TypeId, value: Long)
      extends Package
  case class Operator(
      version: Version,
      typeId: TypeId,
      lengthTypeId: LengthTypeId,
      subPackages: List[Package]
  ) extends Package

}
