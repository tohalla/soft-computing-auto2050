package ga

import scala.util.Random

abstract class Entity {
  val genes: Vector[Any]
  val size = genes.length

  override def toString: String = s"[${genes.mkString(",")}]"
}

case class Genotype(genes: Vector[Float]) extends Entity {
  def decode: Phenotype = new Phenotype(
    genes = genes.map(gene => gene)
  )
}

case class Phenotype(genes: Vector[Float]) extends Entity {
  def encode: Phenotype = new Phenotype(
    genes = genes.map(gene => gene)
  )
}

object Genotype {
  def generate(size: Int): Entity = new Genotype(
    genes = Vector.fill(size)(Random.nextFloat)
  )
}
