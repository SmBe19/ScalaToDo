class ToDo(code_c: String, name_c: String, detail_c: String, project_c: Project, customer_c: Customer) {
  var code: String = code_c
  var name: String = name_c
  var detail: String = detail_c
  var project: Project = project_c
  var customer: Customer = customer_c
  var done: Boolean = false
}
