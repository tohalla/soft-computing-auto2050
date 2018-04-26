package ga

import util.misc

import scala.util.Random

trait Entity {
  val genes: Map[Variable, Float]
  val size: Int = genes.size

  override def toString: String = s"[${genes.mkString(",")}]"
}

case class Genotype(genes: Map[Variable, Float], fitnessValue: Double = 0) extends Entity {
  def decode: Phenotype = Phenotype(genes = genes.map(gene => (gene._1, gene._1.getScaledValue(gene._2))))

  def mutate: Genotype = copy(
    genes = genes.map { case (variable, value) =>
      variable -> (if (Random.nextBoolean) Math.min(1, Math.max(0, value + misc.getGaussianRandom())) else value)
    }
  )

  override def toString: String =
    s"[${genes.map(gene => s"${gene._1.getScaledValue(gene._2)} (${gene._2})").mkString(",")}]. Fitness: $fitnessValue"
}

case class Phenotype(genes: Map[Variable, Float]) extends Entity {
  def encode: Genotype = Genotype(genes = genes.map(gene => (gene._1, gene._1.getValue(gene._2))))

  def generate(variables: Seq[Variable]): Genotype = Genotype(
    genes = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}

object Genotype {
  def generate(variables: Seq[Variable]): Genotype = Genotype(
    genes = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}


