package ga

case class Population(
  entities: Vector[Entity]
) {
  val size = entities.length

  override def toString: String =
    s"""
       |Entities:
       |\t${
      if (size > 10)
        entities.take(2).zipWithIndex.map { case (entity, i) =>
          s"(${i}): ${entity}"
        }.mkString(",\n\t") + s"\n\t...\n\t(${entities.length - 1}): ${entities.last}"
      else entities.zipWithIndex.map{ case (entity, i) => s"(${i}): ${entity}" } mkString (",\n\t")
    }
    """.stripMargin

}

object Population {
  def generatePopulation(size: Int, geneCount: Int) = new Population(
    entities = Vector.fill(size)(Genotype.generate(geneCount))
  )
}