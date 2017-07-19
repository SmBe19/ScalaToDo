import java.io.PrintWriter

class ToDoList(filename: String) {

  protected val fileHeader = "TODO v0.1"

  var shouldQuit: Boolean = false
  protected var inTransaction: Boolean = false

  protected var defaultProject: String = ""
  protected var defaultCustomer: String = ""
  protected var projects: Array[Project] = Array()
  protected var customers: Array[Customer] = Array()

  protected def allToDos: Array[ToDo] = projects.foldLeft(Array[ToDo]())((a, b) => a ++ b.todos)

  val commandFunctions: Map[String, (Array[String]) => Boolean] = Map(
    "exit" -> quit, "quit" -> quit, "q" -> quit,
    "status" -> status,
    "list" -> list, "listproject" -> listProject, "listtodo" -> listToDo, "listcustomer" -> listCustomer,
    "add" -> add, "addproject" -> addProject, "addtodo" -> addToDo, "addcustomer" -> addCustomer,
    "remove" -> remove, "rm" -> remove, "rmproject" -> removeProject, "rmtodo" -> removeToDo, "rmcustomer" -> removeCustomer,
    "transaction" -> transaction,
    "commit" -> commit
  )

  protected def load(): Unit = {
    val f = io.Source.fromFile(filename).getLines()
    if (f.next() != fileHeader) println("warning: wrong file header")

    def f2() = (f.next(), f.next())

    def f6() = (f.next(), f.next(), f.next(), f.next(), f.next(), f.next())

    val Array(p, c, d) = f.next().split(' ').map(x => x.toInt)
    projects = (for (_ <- 1 to p; (code, name) = f2()) yield {
      new Project(code, name)
    }).toArray[Project]
    customers = (for (_ <- 1 to c; (code, name) = f2()) yield {
      new Customer(code, name)
    }).toArray[Customer]
    for (_ <- 1 to d; (code, name, detail, done, project, customer) = f6()) {
      projects.find(x => x.code == project) match {
        case None => println(s"project `$project` not found")
        case Some(proj) => customers.find(x => x.code == customer) match {
          case None => println(s"customer `$project` not found")
          case Some(cust) =>
            val todo = new ToDo(code, name, detail, proj, cust)
            todo.done = done.toBoolean
            proj.todos :+= todo
        }
      }
    }
  }

  protected def write(): Unit = {
    val out = new PrintWriter(filename)
    out.print(fileHeader)
    out.print("\n")
    out.print(s"${projects.length} ${customers.length} ${allToDos.length}")
    out.print("\n")
    projects.foreach(p => out.print(s"${p.code}\n${p.name}\n"))
    customers.foreach(c => out.print(s"${c.code}\n${c.name}\n"))
    allToDos.foreach(d => out.print(s"${d.code}\n${d.name}\n${d.detail}\n${d.done}\n${d.project.code}\n${d.customer.code}\n"))
    out.close()
  }

  protected def writeIfNoTrans(): Unit = if (!inTransaction) write()

  protected def argsOrRead(args: Array[String], prompt: String, default: String): (Array[String], String) = {
    if (args.nonEmpty) {
      (args drop 1, args(0))
    } else {
      val inp = io.StdIn.readLine(if (default == null || default.nonEmpty) s"$prompt [$default]: " else s"$prompt: ")
      (args, if (inp == null || inp.isEmpty) default else inp)
    }
  }

  protected def argsOrReadChain(args: Array[String], promptAndDefaults: (String, String, String => Boolean)*): (Array[String], Seq[String]) = {
    var nargs = args
    var isError = false

    def aux(): Seq[String] = {
      for ((prompt, default, check) <- promptAndDefaults) yield {
        val (nnargs, x) = if (isError) (nargs, "") else argsOrRead(nargs, prompt, default)
        isError ||= !check(x)
        nargs = nnargs
        x
      }
    }

    (nargs, aux())
  }

  def status(args: Array[String]): Boolean = {
    false
  }

  def list(args: Array[String]): Boolean = {
    val (nargs, objtype) = argsOrRead(args, "object type", "todo")
    callCommandOrFail(s"list$objtype", nargs, () => println(s"unknown object `$objtype`"))
  }

  def listProject(args: Array[String]): Boolean = {
    val codeWidth = projects.foldLeft(0)((a, b) => math.max(a, b.code.length))
    val formatString = s"%-${codeWidth + 2}s%s"
    projects.foreach(x => println(formatString.format(x.code, x.name)))
    true
  }

  def listToDo(args: Array[String]): Boolean = {
    val (nargs, project) = argsOrRead(args, "project", defaultProject)
    projects.find(x => x.code == project) match {
      case None if project.isEmpty => listToDo(allToDos); true
      case None => println(s"project `$project` not found"); false
      case Some(proj) => listToDo(proj.todos); true
    }
  }

  protected def listToDo(toDos: Array[ToDo]): Unit = {
    val codeWidth = toDos.foldLeft(0)((a, b) => math.max(a, b.code.length))
    val projWidth = toDos.foldLeft(0)((a, b) => math.max(a, b.project.name.length))
    val custWidth = toDos.foldLeft(0)((a, b) => math.max(a, b.customer.name.length))
    val formatString = s"[%s] %-${projWidth + 2}s%-${custWidth + 2}s%-${codeWidth + 2}s%s"
    toDos.sortWith((a, b) => !a.done && b.done).foreach(x => println(formatString.format(
      if (x.done) "X" else " ",
      x.project.name, x.customer.name, x.code, x.name
    )))
  }

  def listCustomer(args: Array[String]): Boolean = {
    val codeWidth = customers.foldLeft(0)((a, b) => math.max(a, b.code.length))
    val formatString = s"%-${codeWidth + 2}s%s"
    customers.foreach(x => println(formatString.format(x.code, x.name)))
    true
  }

  def add(args: Array[String]): Boolean = {
    val (nargs, objtype) = argsOrRead(args, "object type", "todo")
    callCommandOrFail(s"add$objtype", nargs, () => println(s"unknown object `$objtype`"))
  }

  def addProject(args: Array[String]): Boolean = {
    val (nargs, Seq(code, name)) = argsOrReadChain(args,
      ("code", "", x => !projects.exists(y => y.code == x)),
      ("name", "", _ => true)
    )
    projects.find(x => x.code == code) match {
      case None => projects :+= new Project(code, name); writeIfNoTrans(); true
      case _ => println(s"project `$code` already exists"); false
    }
  }

  def addToDo(args: Array[String]): Boolean = {
    val (nargs, Seq(project, customer, code, name, detail)) = argsOrReadChain(args,
      ("project", defaultProject, x => projects.exists(y => y.code == x)),
      ("customer", defaultCustomer, x => customers.exists(y => y.code == x)),
      ("code", "", _ => true), ("name", "", _ => true), ("detail", "", _ => true)
    )
    projects.find(x => x.code == project) match {
      case None => println(s"project `$project` not found"); false
      case Some(proj) => customers.find(x => x.code == customer) match {
        case None => println(s"customer `$customer` not found"); false
        case Some(cust) => proj.todos.find(x => x.code == code) match {
          case None => proj.todos :+= new ToDo(code, name, detail, proj, cust); writeIfNoTrans(); true
          case _ => println(s"todo `$code` already exists"); false
        }
      }
    }
  }

  def addCustomer(args: Array[String]): Boolean = {
    val (nargs, Seq(code, name)) = argsOrReadChain(args,
      ("code", "", x => !customers.exists(y => y.code == x)),
      ("name", "", _ => true)
    )
    customers.find(x => x.code == code) match {
      case None => customers :+= new Customer(code, name); writeIfNoTrans(); true
      case _ => println(s"customer `$code` already exists"); false
    }
  }

  def remove(args: Array[String]): Boolean = {
    val (nargs, objtype) = argsOrRead(args, "object type", "todo")
    callCommandOrFail(s"rm$objtype", nargs, () => println(s"unknown object `$objtype`"))
  }

  def removeProject(args: Array[String]): Boolean = {
    val (nargs, code) = argsOrRead(args, "code", "")
    projects = projects.filterNot(x => x.code == code)
    writeIfNoTrans()
    true
  }

  def removeToDo(args: Array[String]): Boolean = {
    val (nargs, Seq(project, code)) = argsOrReadChain(args,
      ("project", defaultProject, x => projects.exists(y => y.code == x)),
      ("code", "", _ => true)
    )
    projects.find(x => x.code == project) match {
      case None => println(s"project `$project` not found"); false
      case Some(proj) => proj.todos = proj.todos.filterNot(x => x.code == code); writeIfNoTrans(); true
    }
  }

  def removeCustomer(args: Array[String]): Boolean = {
    val (nargs, code) = argsOrRead(args, "code", "")
    customers = customers.filterNot(x => x.code == code)
    writeIfNoTrans()
    true
  }

  def transaction(args: Array[String]): Boolean = {
    if (inTransaction) commit(args)
    inTransaction = true
    true
  }

  def commit(args: Array[String]): Boolean = {
    if (inTransaction) {
      inTransaction = false
      write()
      true
    } else {
      println("not in transaction")
      false
    }
  }

  def quit(args: Array[String]): Boolean = {
    shouldQuit = true
    true
  }

  def callCommandOrFail(cmd: String, args: Array[String], notFound: () => Unit = () => {}, commandError: () => Unit = () => {}): Boolean = {
    commandFunctions.get(cmd) match {
      case None => notFound(); false
      case Some(f) =>
        if (!f(args)) {
          commandError()
          false
        } else {
          true
        }
    }
  }

  def isValidCommand(cmd: String): Boolean = commandFunctions.contains(cmd)

  load()
}

object ToDoList {

  def isValidCommand(cmd: String): Boolean = new ToDoList("").isValidCommand(cmd)

}