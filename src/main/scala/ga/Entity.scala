package ga

import util.misc

import scala.util.Random

trait Entity {
  val alleles: Map[Variable, Float]
  val size: Int = alleles.size

  override def toString: String = s"[${alleles.mkString(",")}]"
}

case class Genotype(alleles: Map[Variable, Float], fitnessValue: Double = 0) extends Entity {
  // Palauttaa genotyyppiä vastaavan fenotyypin
  def decode: Phenotype = Phenotype(alleles = alleles.map(gene => (gene._1, gene._1.getScaledValue(gene._2))))

  // Palauttaa mutatoidun genotyypin
  def mutate(probability: Float): Genotype = copy(
    // suorittaa jokaiseen genotyypin muuttujaan muotaation tietyllä todennäköisyydellä
    alleles = alleles.map { case (variable, value) =>
      variable -> (
        if (probability >= Random.nextFloat) Math.min(1, Math.max(0, value + misc.getGaussianRandom())) else value
        )
    }
  )

  override def toString: String =
    s"[${alleles.map(gene => s"${gene._1.getScaledValue(gene._2)} (${gene._2})").mkString(",")}]. " +
      s"Fitness: $fitnessValue"
}

case class Phenotype(alleles: Map[Variable, Float]) extends Entity {
  // Palauttaa fenotyyppiä vastaavan genotyypin
  def encode: Genotype = Genotype(alleles = alleles.map(gene => (gene._1, gene._1.getValue(gene._2))))
}

object Genotype {
  def generate(variables: Seq[Variable]): Genotype = Genotype(
    alleles = variables.zip(variables.map(variable => if (variable.isFixed) 1 else Random.nextFloat)).toMap
  )
}


