def square(x: Int) = x * x

square(2)


class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello")
    println("World")
  }
}

def main(): Unit = {
  val t = new HelloThread
  val s = new HelloThread

  t.start()
  s.start()
  t.join()
  s.join()
}

main()
main()
main()


private val x = new AnyRef
private var uidCount = 0L
def getuniqueId(): Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}

//
//def startThread() = {
//  val t = new Thread {
//    override def run(): Unit = {
//      val uids = for(i<-0 to 10) yield getuniqueId()
//      println(uids)
//    }
//  }
//  t.start()
//  t
//}
//
//startThread(); startThread();


class Account(private var amount: Int = 0) {
  val uid = getuniqueId()

  private def lockAndTransfer(target: Account, n: Int): Unit =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }

  def transfer(target: Account, n: Int) =
    if (this.uid < target.uid) this.lockAndTransfer(target, n)
    else target.lockAndTransfer(this, -n)
}

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(500000)
val a2 = new Account(700000)

var t = startThread(a1, a2, 15000)
var s = startThread(a2, a1, 15000)
t.join()
s.join()