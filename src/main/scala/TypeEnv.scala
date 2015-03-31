package bucharestfp

class TypeEnv(bindings: Map[String, TypeScheme]) {
  def get(key: String): Option[TypeScheme] = {
    bindings.get(key)
  }

  def set(key: String, value: TypeScheme): TypeEnv = {
    new TypeEnv(bindings + (key -> value))
  }

  def freeVars: Set[Type.Var] = {
    bindings.values.map(typeScheme => typeScheme.freeVars).toSet.flatten
  }

  def generalize(ty: Type): TypeScheme = {
    val tvars = ty.freeVars -- freeVars
    val r = TypeScheme.forall(tvars, ty)
    println(s"ty: $ty; r: $r")
    r
  }
}

object TypeEnv {
  def empty = new TypeEnv(Map.empty)
}
