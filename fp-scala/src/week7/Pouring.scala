package week7

/**
 * We will develop a fully functional solution to the
 * well known water pouring problem.
 *
 * Problem Statement
 * A classic puzzle starts with two unmarked bottles that can hold
 * M liters and N liters respectively, and a bathtub with unlimited water.
 * How can X liter be measured?
 * Given 0 < X < M or N
 *
 * Problem representation
 *
 * Glass: Int
 * State: Vector[Int] (one entry per glass)
 *
 * Allowed moves (which lead to different states)
 * 1) Empty(glass)
 * 2) Fill(glass)
 * 3) Pour(from, to)
 *
 * So first we try out all possible moves. This will generate an
 * automata of path length 1. Then we try out all moves from
 * every state possible from stage 1, this will generate the second
 * stage of possible states.. and so on. At one point, we will
 * hit our expected state. We select that path. Or there is another
 * case where we have exhausted our search space, and there is no
 * solution.
 *
 * @author Thirumal Venkat
 */
class Pouring(capacity: Vector[Int]) {
  // States
  type State = Vector[Int]

  // Initial states of all the glasses
  val initialState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    def change(state: State) = state.updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    def change(state: State) = state.updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state.updated(from, state(from) - amount).updated(to, state(to) + amount)
    }
  }

  // All glasses
  val glasses = 0 until capacity.length // Glass names

  // All moves possible
  val moves =
    { for (g <- glasses) yield Empty(g) } ++
      { for (g <- glasses) yield Fill(g) } ++
      { for (from <- glasses; to <- glasses if from != to) yield Pour(from, to) }

  // Paths: list of moves in reverse order of history
  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)
    override def toString = (history.reverse mkString " ") + " --> " + endState
  }

  // initial path
  val initialPath = new Path(Nil, initialState)

  // Generate paths
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      // And go on...
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }

  // Path sets
  val pathSets = from(Set(initialPath), Set(initialState))

  // Solution
  def solution(target: Int): Stream[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
  }
}
