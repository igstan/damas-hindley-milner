package bucharestfp

class TypeEnv(bindings: Map[String, Type]) {
  def get(key: String): Option[Type] = {
    bindings.get(key)
  }

  def set(key: String, value: Type): TypeEnv = {
    new TypeEnv(bindings + (key -> value))
  }
}

object TypeEnv {
  def empty = new TypeEnv(Map.empty)
}
