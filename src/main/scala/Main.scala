object Main {

  val defaultFilename = "todo.todo"
  var toDoList: ToDoList = _

  def handleCmd(args: Array[String]): Boolean =
    toDoList.callCommandOrFail(args(0), args drop 1,
      () => println(s"command `${args(0)}` not found"),
      () => println(s"command `${args.mkString(" ")}` failed")
    )

  def interactive(): Unit = {
    val command = io.StdIn readLine "> "
    toDoList.callCommandOrFail(command, Array(),
      () => println(s"command `$command` not found"),
      () => println(s"command `$command` failed")
    )
    if (!toDoList.shouldQuit) interactive()
  }

  def main(args: Array[String]): Unit = {
    val (nargs, filename) =
      if (args.nonEmpty && !ToDoList.isValidCommand(args(0)))
        (args drop 1, args(0))
      else
        (args, defaultFilename)
    toDoList = new ToDoList(filename)
    if (nargs.length > 0) {
      val handleResult = handleCmd(nargs)
      System.exit(if (handleResult) 0 else 1)
    } else {
      interactive()
    }
  }
}
