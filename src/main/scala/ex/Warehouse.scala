package ex

import util.Optionals.Optional
import util.Sequences
import util.Sequences.*
trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item:
  def apply(code: Int, name: String, tags: Sequence[String] = Sequence.empty): Item = ItemImpl(code, name, tags)
case class ItemImpl(val code: Int, val name: String, val tags: Sequence[String]) extends Item
/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

case class WarehouseImpl() extends Warehouse:
  var sequence: Sequence[Item] = Sequence.empty
  override def store(item: Item): Unit =
    this.sequence = this.sequence.concat(Sequence(item))

  override def searchItems(tag: String): Sequence[Item] =
    this.sequence.filter(item => item.tags.contains(tag))

  override def retrieve(code: Int): Optional[Item] =
    val item = this.sequence.find(item => item.code == code)
    this.sequence.filter(item => item.code != code)
    item
  override def remove(item: Item): Unit = ???
object Warehouse:
  def apply(): Warehouse = WarehouseImpl()

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", Sequence("notebook"))
  val dellInspiron = Item(34, "Dell Inspiron 13", Sequence("notebook"))
  val xiaomiMoped = Item(35, "Xiaomi S1", Sequence("moped", "mobility"))

  warehouse.contains(dellXps.code) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  warehouse.contains(dellXps.code) // true
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  warehouse.searchItems("mobility") // List(xiaomiMoped)
  warehouse.searchItems("notebook") // List(dellXps, dell Inspiron)
  warehouse.retrieve(11) // None
  warehouse.retrieve(dellXps.code) // Some(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  warehouse.retrieve(dellXps.code) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/