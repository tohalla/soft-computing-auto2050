package ga

import scala.util.Random

abstract class Entity {
  val genes: Map[Variable, Float]
  val size = genes.size

  override def toString: String = s"[${genes.mkString(",")}]"
}

case class Genotype(genes: Map[Variable, Float]) extends Entity {
  def decode: Phenotype = new Phenotype(
    genes = genes.map(gene => gene)
  )

  def crossover(partner: Genotype, crossoverPoint: Int = Random.nextInt(size)): Genotype =
    if (crossoverPoint >= this.size) this
    else copy(
      genes = genes.keys
        .zip(genes.values.take(crossoverPoint) ++ partner.genes.values.takeRight(size - crossoverPoint))
        .toMap
    )

  def mutate(variable: Variable): Genotype = copy(genes = genes.updated(variable, Random.nextFloat))
}

case class Phenotype(genes: Map[Variable, Float]) extends Entity {
  def encode: Phenotype = new Phenotype(
    genes = genes.map(gene => gene)
  )
}

object Genotype {
  def generate(variables: Set[Variable]): Genotype = new Genotype(
    genes = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}

