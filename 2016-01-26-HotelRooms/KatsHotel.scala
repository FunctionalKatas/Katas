object KatsHotel extends App{

  def findOccupiedRoom(asciiInput:String, occupiedRoomFlag:Char = '*') = {
    val firstOccupiedRoom = firstMatchPos(occupiedRoomFlag)
    val hotelLines = asciiInput.lines.toList.reverse
    val paddedHotelLines = hotelLines.map(_.padTo(hotelLines.head.length, ' '))
    val floorNo = firstOccupiedRoom(paddedHotelLines.transpose.map(_.mkString))
    val roomOnLineNo = firstOccupiedRoom(paddedHotelLines)
    val buildingRoomInfo= paddedHotelLines.head.split("  ").map(_.length).toList
    val buildingAndRoom = extractBuildingAndRoom(buildingRoomInfo, roomOnLineNo)
    s"${buildingAndRoom._1}$floorNo${buildingAndRoom._2}"
  }

  val firstMatchPos : Char => List[String] => Int = matchChar => searchLines =>
    searchLines.filter(_.contains(matchChar))
      .head.indexOf(matchChar) + 1

  def extractBuildingAndRoom(roomInfo: List[Int], roomPos: Int, acc: (Int, Int) = (0, 0)): (Int, Int) =
    roomInfo match {
      case Nil => (-1, -1) //should throw
      case x :: xs =>
        if (roomPos <= acc._2 + x) (acc._1 + 1, roomPos - acc._2)
        else extractBuildingAndRoom(xs, roomPos, (acc._1 + 1, acc._2 + x + 2))
    }

  val ascii1 = "*"
  assert(findOccupiedRoom(ascii1)=="111")

  val ascii2 = "#  #  *  #  #"
  assert(findOccupiedRoom(ascii2)=="311")

  val ascii3 =
    """#####
      |#####
      |####*
      |#####
      |#####""".stripMargin
  assert(findOccupiedRoom(ascii3)=="135")

  val ascii4 =
  """           #
    |           *
    |           #
    |           #
    |           #
    |           #
    |           #
    |           #
    |#########  #  #""".stripMargin
  assert(findOccupiedRoom(ascii4)=="281")

  val ascii5 =
  """#
    |#  #
    |#  #  ##
    |#  #  ##  ###
    |#  #  ##  ###  #####
    |#  #  ##  ###  ##*##  ########
    |#  #  ##  ###  #####  ########""".stripMargin
  assert(findOccupiedRoom(ascii5)=="523")

  val ascii6 =
  """           #
    |           *
    |           #
    |           #
    |           #
    |           #
    |           #
    |           #
    |#########  #  #""".stripMargin
  assert(findOccupiedRoom(ascii6)=="281")

  val ascii7 =
  """                        ########*
    |                        #########
    |                        #########
    |                        #########
    |                        #########
    |                        #########
    |                        #########
    |                        #########
    |#  #  #  #  #  #  #  #  #########""".stripMargin
  assert(findOccupiedRoom(ascii7)=="999")
}
