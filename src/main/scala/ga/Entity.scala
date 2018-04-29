package ga

import util.misc

import scala.util.Random

trait Entity {
  val alleles: Map[Variable, Float]
  val size: Int = alleles.size

  override def toString: String = s"[${alleles.mkString(",")}]"
}

case class Genotype(alleles: Map[Variable, Float], fitnessValue: Double = 0) extends Entity {
  def decode: Phenotype = Phenotype(alleles = alleles.map(gene => (gene._1, gene._1.getScaledValue(gene._2))))

  def mutate: Genotype = copy(
    alleles = alleles.map { case (variable, value) =>
      variable -> (if (Random.nextBoolean) Math.min(1, Math.max(0, value + misc.getGaussianRandom())) else value)
    }
  )

  override def toString: String =
    s"[${alleles.map(gene => s"${gene._1.getScaledValue(gene._2)} (${gene._2})").mkString(",")}]. Fitness: $fitnessValue"
}

case class Phenotype(alleles: Map[Variable, Float]) extends Entity {
  def encode: Genotype = Genotype(alleles = alleles.map(gene => (gene._1, gene._1.getValue(gene._2))))

  def generate(variables: Seq[Variable]): Genotype = Genotype(
    alleles = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}

object Genotype {
  def generate(variables: Seq[Variable]): Genotype = Genotype(
    alleles = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}


