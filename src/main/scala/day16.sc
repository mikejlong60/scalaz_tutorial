import scalaz.Memo

val slowFib: Int => Int = {
  case 0 => 0
  case 1 => 1
  case n => slowFib(n - 2) + slowFib(n -1)
}

slowFib(40)

val memoizedFib: Int => Int = Memo.mutableHashMapMemo {
  case 0 => 0
  case 1 => 1
  case n => memoizedFib(n - 2) + memoizedFib(n -1)
}
memoizedFib(75)

