object  Day01 :
    trait Location(val north: Int, val east: Int, val positions: Seq[(Int, Int)]) {
        def move(command: String): Location
    }

    class North(north: Int, east: Int, positions: Seq[(Int, Int)] = Seq.empty) extends Location(north, east, positions){
        override def move(command: String): Location = {
            val distance: Int = command.drop(1).toInt
            val direction = command.charAt(0)
            val newPositions = track(distance.abs, direction, positions)
            direction match
                case 'R' => East(north, east + distance, newPositions)
                case _ => West(north, east - distance, newPositions)
        }
        def track(distance: Int, direction: Char, newPositions: Seq[(Int, Int)]): Seq[(Int, Int)] = {
            if distance <= 0 then newPositions
            else {
                val currentPosition = newPositions.last
                direction match
                    case 'R' => track(distance - 1, direction, newPositions.:+ (currentPosition(0), currentPosition(1) + 1))
                    case _ => track(distance - 1, direction, newPositions.:+ (currentPosition(0), currentPosition(1) - 1))
            }
        }
    }

    class East(north: Int, east: Int, positions: Seq[(Int, Int)] = Seq.empty) extends Location(north, east, positions) {
        override def move(command: String): Location = {
            val distance: Int = command.drop(1).toInt
            val direction = command.charAt(0)
            val newPositions = track(distance.abs, direction, positions)
            direction match
                case 'R' => South(north - distance, east, newPositions)
                case _ => North(north + distance, east, newPositions)
        }
        def track(distance: Int, direction: Char, newPositions: Seq[(Int, Int)]): Seq[(Int, Int)] = {
            if distance <= 0 then newPositions
            else {
                val currentPosition = newPositions.last
                direction match
                    case 'R' => track(distance - 1, direction, newPositions.:+(currentPosition(0) - 1, currentPosition(1)))
                    case _ => track(distance - 1, direction, newPositions.:+(currentPosition(0) + 1, currentPosition(1)))
            }
        }
    }

    class South(north: Int, east: Int, positions: Seq[(Int, Int)] = Seq.empty) extends Location(north, east, positions) {
        override def move(command: String): Location = {
            val distance: Int = command.drop(1).toInt
            val direction = command.charAt(0)
            val newPositions = track(distance.abs, direction, positions)
            direction match
                case 'R' => West(north, east - distance, newPositions)
                case _ => East(north, east + distance, newPositions)
        }
        def track(distance: Int, direction: Char, newPositions: Seq[(Int, Int)]): Seq[(Int, Int)] = {
            if distance <= 0 then newPositions
            else {
                val currentPosition = newPositions.last
                direction match
                    case 'R' => track(distance - 1, direction, newPositions.:+(currentPosition(0), currentPosition(1) - 1))
                    case _ => track(distance - 1, direction, newPositions.:+(currentPosition(0), currentPosition(1) + 1))
            }
        }
    }

    class West(north: Int, east: Int, positions: Seq[(Int, Int)] = Seq.empty) extends Location(north, east, positions) {
        override def move(command: String): Location = {
            val distance: Int = command.drop(1).toInt
            val direction = command.charAt(0)
            val newPositions = track(distance.abs, direction, positions)
            direction match
                case 'R' => North(north + distance, east, newPositions)
                case _ => South(north - distance, east, newPositions)
        }
        def track(distance: Int, direction: Char, newPositions: Seq[(Int, Int)]): Seq[(Int, Int)] = {
            if distance <= 0 then newPositions
            else {
                val currentPosition = newPositions.last
                direction match
                    case 'R' => track(distance - 1, direction, newPositions.:+(currentPosition(0) + 1, currentPosition(1)))
                    case _ => track(distance - 1, direction, newPositions.:+(currentPosition(0) - 1, currentPosition(1)))
            }
        }
    }

    def firstDuplicate(positions: Seq[(Int, Int)], seen: Seq[(Int, Int)] = Seq.empty): (Int, Int) = {
        if seen.contains(positions.head) then positions.head
        else firstDuplicate(positions.tail, seen.appended(positions.head))
    }

    def part1(input: String): Int = {
        val initial: Location = North(0, 0, Seq((0,0)))
        val moves = input.split(", ").foldLeft(initial)((location, command) => {location.move(command)})

        moves.north.abs + moves.east.abs
    }

    def part2(input: String): Int = {
        val initial: Location = North(0, 0, Seq((0,0)))
        val positions = input.split(", ").foldLeft(initial)((location, command) => {location.move(command)}).positions
        val duplicate = firstDuplicate(positions)

        duplicate(0).abs + duplicate(1).abs
    }


    def main(args: Array[String]): Unit = {
        val data = io.Source.fromResource("Day01.txt").mkString.trim
        println(part1(data))
        println(part2(data))
    }
