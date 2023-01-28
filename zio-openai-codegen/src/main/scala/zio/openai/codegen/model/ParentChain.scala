package zio.openai.codegen.model

import zio.Chunk

final case class ParentChain(items: Chunk[String]) {
  val name: Option[String] =
    if (items.isEmpty) None
    else Some(items.mkString("_"))

  def /(name: String): ParentChain =
    copy(items = items :+ name)
}

object ParentChain {
  val empty: ParentChain = ParentChain(Chunk.empty)
}
