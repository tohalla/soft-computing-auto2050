package ga

case class Population(genotypes: Vector[Genotype]) {
  val size: Int = genotypes.length

  // Palauttaa skaalatun populaation
  def resize(newSize: Int): Population =
    if (size == newSize) this
    else if (size < newSize) // jos tarve kasvattaa populaatiota, lisätään satunnaisia alkioita
      copy(genotypes = genotypes ++ Vector.fill(newSize - size)(Genotype.generate(genotypes.head.alleles.keySet.toSeq)))
    else copy(genotypes = genotypes.take(newSize)) // ... muutoin valitaan alkupäästä uuden populaation koon verran alkioita

  override def toString: String = toString(true)

  // funktio alkioiden tulostamista varten
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
  /**
    * @param size Uuden populaation koko
    * @param variables Lista alkioiden sisältämistä muuttujista
    * @param getFitnessValue Funktio kelvollisuusarvon laskua varten
    * @return
    */
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