package ga

import scala.util.Random

trait Entity {
  val genes: Map[Variable, Float]
  val size: Int = genes.size

  override def toString: String = s"[${genes.mkString(",")}]"
}

case class Genotype(genes: Map[Variable, Float], fitnessValue: Double = 0) extends Entity {
  def decode: Phenotype = Phenotype(genes = genes.map(gene => (gene._1, gene._1.getScaledValue(gene._2))))

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
  def encode: Genotype = Genotype(genes = genes.map(gene => (gene._1, gene._1.getValue(gene._2))))

  def generate(variables: Set[Variable]): Genotype = Genotype(
    genes = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}

object Genotype {
  def generate(variables: Set[Variable]): Genotype = Genotype(
    genes = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}


