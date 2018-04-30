package ga

case class Population(genotypes: Vector[Genotype]) {
  val size: Int = genotypes.length

  def resize(newSize: Int): Population =
    if (size == newSize) this
    else if (size < newSize)
      copy(genotypes = genotypes ++ Vector.fill(newSize - size)(Genotype.generate(genotypes.head.alleles.keySet.toSeq)))
    else copy(genotypes = genotypes.take(newSize))

  override def toString: String = toString(true)

  def toString(truncate: Boolean = true): String = {
    val sorted = genotypes.sortWith(_.fitnessValue > _.fitnessValue)
    s"""
       |Alkiot:
       |\t${
      if (size > 10 && truncate)
        sorted
          .take(2)
          .zipWithIndex
          .map { case (genotype, i) => s"($i): $genotype" }
          .mkString(",\n\t") + s"\n\t...\n\t(${genotypes.length - 1}): ${sorted.last}"
      else sorted.zipWithIndex.map { case (genotype, i) => s"($i): $genotype" } mkString ",\n\t"
    }
    """.stripMargin
  }
}

object Population {
  def generatePopulation(
    size: Int,
    variables: Seq[Variable],
    getFitnessValue: Option[(Phenotype, Seq[Variable]) => Double] = None
  ) = Some(Population(genotypes = Vector.fill(size)({
    val genotype = Genotype.generate(variables)
    if (getFitnessValue.isDefined) genotype.copy(fitnessValue = getFitnessValue.get(genotype.decode, variables))
    else genotype
  })))
}