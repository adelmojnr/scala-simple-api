package controllers

import models.{ListItem, SendItem}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.{Inject, Singleton}
import scala.collection.mutable
@Singleton
class ListController @Inject() (val controllerComponents: ControllerComponents)
    extends BaseController {
  private var list = new mutable.ListBuffer[ListItem]()
  list += ListItem(1, "Livro", true)
  list += ListItem(2, "Ipad", true)
  list += ListItem(3, "Laptop", true)
  implicit val jsonList = Json.format[ListItem]
  implicit val newItem = Json.format[SendItem]

  def getAll(): Action[AnyContent] = Action {
    if (list.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(list))
    }
  }

  def getById(id: Long): Action[AnyContent] = Action {
    val item = list.find(_.id == id);
    item match {
      case Some(item) => Ok(Json.toJson(item))
      case None       => NotFound
    }
  }

  def sendItem() = Action { implicit request =>
    val data = request.body.asJson
    val item: Option[SendItem] = data.flatMap(Json.fromJson[SendItem](_).asOpt)
    item match {
      case Some(newItem) =>
        val nextId = list.map(_.id).max + 1
        val toBeAdd = ListItem(nextId, newItem.description, false);
        list += toBeAdd
        Created(Json.toJson(toBeAdd))
      case None => BadRequest
    }
  }

  def updateItem(id: Long): Action[AnyContent] = Action { implicit request =>
    val item = list.find(_.id == id);
    val json: JsValue = Json.parse("""{"message": "ID não encontrado"}""")
    item match {
      case Some(property) =>
        val newValue = property.copy(done = true)
        list.dropWhileInPlace(_.id == id)
        list += newValue
        Accepted(Json.toJson(newValue))
      case None => BadRequest(Json.toJson(json))
    }

  }
}
