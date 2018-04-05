package ga

import scala.util.Random

case class Entity(
  genes: Vector[Any]
) {
  val size = genes.length

  // genotyyppi -> fenotyyppi
  def decode: Entity = copy(
    genes = genes.map(gene => gene)
  )

  // fenotyyppi -> genotyyppi
  def encode: Entity = copy(
    genes = genes.map(gene => gene)
  )

  override def toString: String = s"[${genes.mkString(",")}]"
}

object Entity {
  def generateEntity(size: Int): Entity = new Entity(
    genes = Vector.fill(size)(Random.nextFloat)
  )
}
