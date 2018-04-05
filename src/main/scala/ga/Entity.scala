package ga

import scala.util.Random

abstract class Entity {
  val genes: Vector[Float]
  val size = genes.length

  override def toString: String = s"[${genes.mkString(",")}]"
}

case class Genotype(genes: Vector[Float]) extends Entity {
  def decode: Phenotype = new Phenotype(
    genes = genes.map(gene => gene)
  )

  def crossover(partner: Genotype, crossoverPoint: Int = Random.nextInt(size)): Genotype =
    if (crossoverPoint >= this.size) this else copy(
      genes = genes.take(crossoverPoint) ++ partner.genes.takeRight(size - crossoverPoint)
    )

  def mutate(location: Int = Random.nextInt(size)): Genotype =
    if (location >= this.size) this else copy(
      genes = genes.updated(location, Random.nextFloat)
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
