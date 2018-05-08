package ga

import scala.annotation.tailrec
import scala.util.Random

trait ParentSelection {
  // Palauttaa ehdokkaat, joista voidaan edelleen johtaa seuraavan sukupolven jäsenet
  def getParents(candidates: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)]

  def prepare(genotypes: Vector[Genotype]): Vector[Genotype] = genotypes

  protected def sort(genotypes: Vector[Genotype]): Vector[Genotype] =
    genotypes.sortWith(_.fitnessValue > _.fitnessValue)

  // Suoritetaan, kunnes genotyyppi löytyy. Jos aikaisempia ei päädytä valitsemaan, palautetaan viimeinen genotyyppi
  @tailrec
  protected final def drawParent(candidatesWithProbabilities: Vector[(Genotype, Double)], i: Int = 0): Genotype =
    if (i < candidatesWithProbabilities.length - 1 && candidatesWithProbabilities(i)._2 >= Random.nextFloat)
      drawParent(candidatesWithProbabilities, i + 1)
    else {
      candidatesWithProbabilities(i)._1
    }
}

object RouletteSelection extends ParentSelection {
  // kelvollisuusarvo positiiviseksi
  override def prepare(genotypes: Vector[Genotype]): Vector[Genotype] = {
    genotypes.map(genotype => genotype.copy(
      fitnessValue = genotype.fitnessValue + Math.abs(genotypes.last.fitnessValue)
    ))
  }

  override def getParents(genotypes: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)] = {
    val candidates = prepare(genotypes)
    val fitnessSum = candidates.foldLeft(0d)(_ + _.fitnessValue)
    val candidatesWithProbabilities = candidates.map(candidate => (candidate, candidate.fitnessValue / fitnessSum))
    Vector.fill(parentCount)((drawParent(candidatesWithProbabilities), drawParent(candidatesWithProbabilities)))
  }
}

object RankSelection extends ParentSelection {
  override def getParents(genotypes: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)] = {
    val rankSum = (genotypes.length + 1) * genotypes.length / 2 // Indeksien yhteenlaskettu summa
    val candidatesWithProbabilities = genotypes.zip(Vector.tabulate(genotypes.length)(i => (i + 1d) / rankSum).reverse)
    Vector.fill(parentCount)((drawParent(candidatesWithProbabilities), drawParent(candidatesWithProbabilities)))
  }
}

object RankedTournamentSelection extends ParentSelection {
  val tournamentSize: Int = 4

  override def getParents(genotypes: Vector[Genotype], parentCount: Int): Vector[(Genotype, Genotype)] = {
    val rankSum = (tournamentSize + 1) * tournamentSize / 2 // Indeksien yhteenlaskettu summa
    val probabilities = Vector.tabulate(tournamentSize)(i => (i + 1d) / rankSum).reverse
    Vector.fill(parentCount)({
      val candidatesWithProbabilities = sort(Vector.fill(tournamentSize)(genotypes(Random.nextInt(genotypes.length))))
        .zip(probabilities)
      (drawParent(candidatesWithProbabilities), drawParent(candidatesWithProbabilities))
    })
  }

}

