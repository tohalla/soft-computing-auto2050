package ga

case class Population(genotypes: Vector[Genotype]) {
  val size = genotypes.length

  def resize(newSize: Int): Population =
    if (size == newSize) this
    else if (size < newSize)
      copy(genotypes = genotypes ++ Vector.fill(newSize - size)(Genotype.generate(genotypes.head.size)))
    else copy(genotypes = genotypes.take(newSize))

  override def toString: String =
    s"""
       |Entities:
       |\t${
      if (size > 10)
        genotypes.take(2).zipWithIndex.map { case (genotype, i) =>
          s"(${i}): ${genotype}"
        }.mkString(",\n\t") + s"\n\t...\n\t(${genotypes.length - 1}): ${genotypes.last}"
      else genotypes.zipWithIndex.map { case (genotype, i) => s"(${i}): ${genotype}" } mkString (",\n\t")
    }
    """.stripMargin
}

object Population {
  def generatePopulation(size: Int, geneCount: Int) = new Population(
    genotypes = Vector.fill(size)(Genotype.generate(geneCount))
  )
}