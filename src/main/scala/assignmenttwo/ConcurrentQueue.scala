package assignmenttwo
import java.util.concurrent.atomic.AtomicReference

/*
 * Lock less queue with dummy node as the head to prevent dequeue from accessing the head, and enqueue
 * from accessing the tail pointer.
 *
 * MPMC : Multi-producer, multi-consumer queue.
 */
object ConcurrentQueue {
  def apply[T]() : ConcurrentQueue[T] = new ConcurrentQueue[T]()
  def apply[T](e : T*) : ConcurrentQueue[T] = {
    val q  = new ConcurrentQueue[T]()
    for(element <- e){
      q.enqueue(element)
    }
    q
  }
}


class ConcurrentQueue[T]{
  def iter = new QIter[T](this)
  val dummy : Node[T] = new Node[T](next = null)
  val head : AtomicReference[Node[T]] = new AtomicReference[Node[T]](dummy)
  val tail : AtomicReference[Node[T]] =  new AtomicReference[Node[T]](dummy)

  def dequeue() : Option[T] = {
    /*
     *                                          Bad Conditions
     * The next node is not null but the CAS fails then retry, because the operation was interleaved
     * by some other thread.
     *
     * The next node is null then exit the loop and do not attempt a CAS.
     *
     * */

    /*
     *                                          Good Conditions
     *
     * The next node is not null and CAS the head to that node is successful then exit the try-CAS.
     *
     *
     * */
    var headNode : Node[T] = null
    var valueNode : Node[T] = null

    do {
      //Get the head node and it's next.
      headNode = head.get()
      valueNode = headNode.next
    } while (valueNode != null && !head.compareAndSet(headNode, valueNode))

    /*
     *                                            After Try CAS
     * Do a null check on head's next node to prevent a NullPointerException.
     *
     */

    try{

      /*
       * If valueNode's value is null then don't throw exception, just return None as the value.
       * */
      val value = Option(valueNode.value)
      /*
       * If the valueNode is not null then set it's value to null to keep it dummy.
       * */
      if(valueNode != null) valueNode.value = None

      /*
       * Return an option of the value.
       * Catch a NullPointerException thrown by valueNode.value and return None.
       */
      value.get
    } catch {
      case npe : NullPointerException => None
    }
  }

  def enqueue(element : T) : Unit = {
    val newNode = new Node[T](Some(element), null)
    val prevTailNode = tail.getAndSet(newNode)
    prevTailNode.next = newNode
  }

  def isEmpty : Boolean = {
    println(head.get())
    head.get().next != null
  }

  def peek : Boolean = {
    var headNode : Node[T] = null
    var valueNode : Node[T] = null
    //Get the head node and it's next.
    headNode = head.get()
    valueNode = headNode.next
    var containsValue : Boolean = false
    if(valueNode == null){
      containsValue = false
    }
    else if(valueNode.value != null){
      containsValue = true
    }
    containsValue
  }
}

class QIter[T](q : ConcurrentQueue[T]) extends Iterator[T]{

  override def hasNext : Boolean = {
    /*
     *                                          Bad Conditions
     * The next node is not null but the CAS fails then retry, because the operation was interleaved
     * by some other thread.
     *
     * The next node is null then exit the loop and do not attempt a CAS.
     *
     * */

    /*
     *                                          Good Conditions
     *
     * The next node is not null and CAS the head to that node is successful then exit the try-CAS.
     *
     *
     * */
    var headNode : Node[T] = null
    var valueNode : Node[T] = null

    do {
      //Get the head node and it's next.
      headNode = q.head.get()
      valueNode = headNode.next
    } while (valueNode != null && !q.head.compareAndSet(headNode, valueNode))

    /*
     *                                            After Try CAS
     * Do a null check on head's next node to prevent a NullPointerException.
     *
     */

    try{
      /*
       * If valueNode's value is null then don't throw exception, just return None as the value.
       * */
      val value = Option(valueNode.value)
      /*
       * If the valueNode is not null then set it's value to null to keep it dummy.
       * */
      if(valueNode != null) valueNode.value = None

      /*
       * Return an option of the value.
       * Catch a NullPointerException thrown by valueNode.value and return None.
       */
      if(value.get.isDefined){
        _next.set(value.get.get)
        true
      }
      else false
    } catch {
      case npe : NullPointerException => false
    }
  }
  val _next : AtomicReference[T] = new AtomicReference[T]()
  override def next(): T = _next.get()
}
