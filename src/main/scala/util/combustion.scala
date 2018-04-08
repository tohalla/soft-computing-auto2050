package util

object combustion {
  def getNetCalorificValue(
    water: Float,
    coal: Float,
    hydrogen: Float,
    sulphur: Float,
    nitrogen: Float,
    oxygen: Float,
    ash: Float
  ): Float =
    getGrossCalorificValue(coal, hydrogen, sulphur, nitrogen, oxygen, ash) * (1f - water) -
      2.444f * (water + 8.936f * hydrogen * (1f - water))

  def getGrossCalorificValue(
    coal: Float,
    hydrogen: Float,
    sulphur: Float,
    nitrogen: Float,
    oxygen: Float,
    ash: Float
  ): Float =
    0.3491f * coal + 1.1783f * hydrogen + 0.1005f * sulphur - 0.0151f * nitrogen - 0.1034f * oxygen - 0.0211f * ash
}
