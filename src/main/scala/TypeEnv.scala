package bucharestfp

class TypeEnv(bindings: Map[String, Type]) {
  def lookup(key: String): Option[Type] = {
    bindings.get(key)
  }

  def set(key: String, value: Type): TypeEnv = {
    new TypeEnv(bindings + (key -> value))
  }
}
