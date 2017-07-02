package gui

import scala.swing._
import scala.swing.BorderPanel.Position
import scala.swing.event._
import javax.swing.Timer
import gameLogic._
import gameUnits._
import javax.swing.border.MatteBorder
import javax.swing.UIManager
import java.awt.Color


object MainWindow extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
 
  TowerCreator.createTowers()
  EnemyCreator.createEnemies()
  
  /* Default values and variables declared here */
  
  private var backgroundPic: javax.swing.ImageIcon = new javax.swing.ImageIcon("Pics/menuBg.png")
  val defaultBorder = new MatteBorder(1, 1, 1, 1, Color.BLACK)
  var towerShot: Seq[(Tower, Seq[Enemy])] = Array[(Tower, Seq[Enemy])]()
  def singleShots = towerShot.collect{ case (t: ShootingTower, e: Seq[Enemy]) => (t, e.head) }
  def areaShots = towerShot.collect{ case (t: AreaTower, _) => t }
  def paused: Boolean = !Game.timer.isRunning()
  
  var towerSelected: Option[Tower] = None    // Is the tower that is currently being "placed"
  var towerClicked: Option[Tower] = None     // Is the tower that is currently clicked; either the same as towerSelected or a tower already placed
  def towerHovered: Option[Tower] = course.isOnTower(mousePos)
  var mousePos = Coord(0, 0)
  
  def course = Game.course
  def path = course.getPath
  def player = Game.player
  
  
  /* Removes all previous values in variables used for drawing */
  
  def reset() = {
    towerSelected = None
    towerClicked = None
    towerShot = Array[(Tower, Seq[Enemy])]()
    speedSlider.value = 1
    autoStartButton.selected = false
  }
  
  /* Variable declarations end here */
  
  def dim(x: Int, y: Int) = new Dimension(x, y)
  
  val gameBoard = new Panel { 
    preferredSize = dim(500, 500)
    override def paintComponent(g: Graphics2D) = {
      /* draw background */
      g.drawImage(course.getBackground.getImage, 0, 0, null)
      /* draw path (for testing?) */
      val pathx = path.map(_.xI).toArray
      val pathy = path.map(_.yI).toArray
      g.drawPolyline(pathx, pathy, pathx.length)
      /* draw enemies */
      course.enemies.foreach(e => Drawing.drawEnemy(g, e.location.xI, e.location.yI, e.maxHealth, e.currentHealth, e.size))
       /* draw towers */
      course.towers.foreach(t => {
        Drawing.setTowerColor(g, t)
        Drawing.drawTower(g, t.location.xI, t.location.yI, t.size)
      })
      g.setColor(Color.BLACK)
      /* draw shots */
      for (p <- singleShots) {
        val t = p._1
        val e = p._2
        Drawing.drawShot(g, t.location.xI, t.location.yI, e.location.xI, e.location.yI)
      }
      /* draw area tower shots */
      for (t <- areaShots) {
        Drawing.drawArea(g, t.location.xI, t.location.yI, t.range)
      }
      /* move selected tower around */
      towerSelected.foreach({t =>
        Drawing.setTowerColor(g, t)
        if (!course.isLegal(mousePos, t) || !Game.canAfford(t))
          g.setColor(Color.RED)
        Drawing.drawTower(g, mousePos.xI, mousePos.yI, t.size, rangeEnabled = true, range = t.range)
        g.setColor(Color.BLACK)
      })
      /* show range when hovering or selected */
      if (towerSelected.isEmpty)
        (towerHovered ++ towerClicked).foreach(t => {
          Drawing.setTowerColor(g, t)
          Drawing.drawTower(g, t.location.xI, t.location.yI, t.size, true, t.range)
          g.setColor(Color.BLACK)
        })
    }
  }
  
  val towerBar = new GridPanel(2,5) {
    /* declare size info */
    val width = 200
    val height = 400
    val towers: Seq[Tower] = TowerCreator.towerTypes.take(10)
    def boxesVer = {
      if (towers.length <= 5) towers.length
      else (towers.length + 1) / 2
    }
    def boxesHor = {
      if (towers.length <= 5) 1
      else 2
    }
    val boxHeight = height / boxesVer
    val boxWidth  = width / boxesHor
    preferredSize = dim(width,height)
    border = defaultBorder
    val towerSize = 25
    /* A method that returns the middle point of a box in the grid */
    def boxMiddle(i: Int): Coord = {          //parameter is the box number    0  1
      Coord((width / boxesHor / 2)  + (i % boxesHor) * boxWidth,          //   2  3 etc...
            (height / boxesVer / 2) + (i / boxesHor) * boxHeight)
    }

    override def paintComponent(g: Graphics2D) = {
      Drawing.drawGrid(g, boxesHor, boxesVer, boxWidth, boxHeight, 0, 0)
      for (i <- 0 until towers.length) {
        Drawing.setTowerColor(g, towers(i))
        Drawing.drawTower(g, boxMiddle(i).xI, boxMiddle(i).yI, towerSize)
        g.setColor(Color.BLACK)
        /* Some attempt at making the text look good */
        g.drawString("Cost: " + towers(i).cost.toString, boxMiddle(i).xI - towerSize / 2 - boxWidth / 8, boxMiddle(i).yI + towerSize / 2 + (boxHeight + towerSize) / 6)
        g.drawString(towers(i).name, boxMiddle(i).xI - towerSize / 2 - boxWidth / 3, boxMiddle(i).yI - towerSize / 2 - (boxHeight + towerSize) / 8)
      }
    }
  }
  
  /* TextLabels declared here */
  val lives = new TextLabel {
    preferredSize = dim(100, 25)
    override def updateText() = 
      text = "Lives: " + player.lives
    text = "Lives: " + player.lives
  }
  
  val money = new TextLabel {
    preferredSize = dim(100, 25)
    override def updateText() = 
      text = "Money: " + player.money
    text = "Money: " + player.money
  }
  
  val roundCounter = new TextLabel {
    preferredSize = dim(100, 25)
    override def updateText() = 
      text = "Round " + course.roundNo + " / " + course.rounds.length
    text = "Round " + course.roundNo + " / " + course.rounds.length
  }
  
  val towerInfo = new GridPanel(3,2) {
    preferredSize = dim(300, 50)
    val towerRange = new TextLabel {
      override def updateText = 
        text = "Range: " + {for (t <- towerClicked) yield t match { 
          case d: DamageTower => d.range
          case _ => "-"
          } 
        }.getOrElse("")
      text = "Range: "
    }
    val towerDmg = new TextLabel {
      override def updateText = 
        text = "Damage: " + { for (t <- towerClicked) yield t match { 
          case d: DamageTower => d.dmg 
          case _ => "-"
          } 
        }.getOrElse("")
      text = "Damage: "
    }
    val towerSpeed = new TextLabel {
      override def updateText: Unit = {
        text = "Speed: " + { for (t <- towerClicked) yield t match { 
          case d: DamageTower => f"${60.0 / d.speed}%.2f"
          case _ => "-"
          } 
        }.getOrElse("")
      }
      text = "Speed: "
    }
    val towerCost = new TextLabel {
      override def updateText = 
        text = "Cost: " + {for (t <- towerClicked) yield t.cost}.getOrElse("")
      text = "Cost: "
    }
    val towerIncome = new TextLabel {
      override def updateText() = 
        text = "Income: " + {for (t <- towerClicked) yield t match { 
          case m: MoneyTower => m.income
          case _ => "-"
          } 
        }.getOrElse("")
        text = "Income: "
    }
    val totalIncome = new TextLabel {
      override def updateText() = 
        text = "Total income: " + course.moneyTowers.map(_.income).sum
      text = "Total income: 0"
    }
    contents += (towerRange, towerCost, towerDmg, towerIncome, towerSpeed, totalIncome)
  }
  /* TextLabel declarations end here */
  
  /* Buttons declared here */  
  val pauseButton = new Button {
    preferredSize = dim(100, 25)
    def updateText(): Unit = 
      text = 
        if (course.roundNo == 0) "Start" 
        else { 
          if (paused) "Resume" 
          else "Pause"
        }
    text = "Start"
  }
  
  val sellButton = new Button {
    preferredSize = dim(100, 50)
    def updateText() = 
      text = "Sell (" + {for (t <- towerClicked) yield t.cost / 2}.getOrElse("0") + ")"
    text = "Sell (0)"
    enabled = towerClicked.isDefined
  }
  val nextRoundButton = new Button {
    preferredSize = dim(100 , 25)
    text = "Next round"
  }
  
  val autoStartButton = new ToggleButton {
    preferredSize = dim(100, 25)
    text = "Auto-start"
  }
  
  val targetFirst = new TargetButton(First)("First")
  val targetLast = new TargetButton(Last)("Last")
  val targetStrong = new TargetButton(Strong)("Strong")
  val targetWeak = new TargetButton(Weak)("Weak")
  
  val targetButtons = TargetButton.buttons
  val targetSelector = new GridPanel(2,2) {
    targetButtons.foreach(contents += _)
  }
  
  val playButton = new Button("Play") {
    preferredSize = dim(250, 50)
  }
  /* Button declarations end here */
  
  /* Speed slider here */
  val speedSlider = new Slider {
    val pairs = for (i <- min to max) yield (i -> new Label(i + "x"))
    labels = pairs.toMap
    min = 1
    max = 8
    majorTickSpacing = 1
    value = 1
    paintTicks = true
    paintLabels = true
    preferredSize = dim(200,50)
  }
  /* Speed slider ends here */
  
  /* Screen layout declared here */
  val info = new BoxPanel(Orientation.Vertical) {
    val width = 200
    val height = 100
    preferredSize = dim(width, height)
    border = defaultBorder
    val topRow = new GridPanel(2, 2) {
      contents += nextRoundButton
      contents += autoStartButton
      contents += roundCounter
      contents += pauseButton
    }
    contents += topRow
    contents += speedSlider
    override def paintComponent(g: Graphics2D) {
    } 
  }

  val sideBar = new BoxPanel(Orientation.Vertical) {
    preferredSize = dim(200, 500)
    contents += towerBar
    contents += info
  }

  val bottomBar = new BoxPanel(Orientation.Horizontal) {
    preferredSize = dim(700,50)
    val livesAndMoney = new BoxPanel(Orientation.Vertical) {contents += lives; contents += money}
    border = defaultBorder
    contents += towerInfo
    contents += livesAndMoney
    contents += sellButton
    contents += targetSelector
  }
  
  val gameScreen = new BorderPanel {
    layout(gameBoard) = Position.Center
    layout(sideBar) = Position.East
    layout(bottomBar) = Position.South
  }
  
  val menuScreen = new GridBagPanel {
    override def paintComponent(g: Graphics2D) = g.drawImage(backgroundPic.getImage, 0, 0, null)
    preferredSize = gameBoard.preferredSize
    val c = new Constraints
    layout(playButton) = c
  }
  
  val levelSelectScreen = new GridBagPanel {
    override def paintComponent(g: Graphics2D) = g.drawImage(backgroundPic.getImage, 0, 0, null)
    preferredSize = gameBoard.preferredSize
    val buttonHolder = new GridPanel(3, 5) {preferredSize = dim(750, 250)}
    val c = new Constraints
    val buttons = for (course <- Game.courses) yield new Button {
      text = "Level: " + course.name
      preferredSize = dim(250, 50)
    }
    for (b <- buttons) buttonHolder.contents += b
    layout(buttonHolder) = c

  }
  /* Screen layout is done here */
  
  /* Methods declared here */
  def sell(t: Tower) = {
    course.removeTower(t)
    player.addMoney(t.cost / 2)
    towerClicked = None
  }
  
  def updateTexts() = {
    TextLabel.labels.foreach(_.updateText())
    sellButton.updateText()
    pauseButton.updateText()
  }
  
  /* Displays a pop-up window saying if the player won or lost and clears all previous drawings */
  def gameOver(victory: Boolean) {
    var text = ""
    var title = ""
    if (Game.loss) {
      title = "Game over"
      text = "Too bad, you lost..."
    } else {
      title = "Victory!"
      text = "Congratulations, you won!"
    }
    val ok = Dialog.showMessage(gameScreen, text, title, Dialog.Message.Info)
    frame.contents = menuScreen
    reset()
    Game.timer.stop()
    update()
  }
  
  /* Main update method, called on each tick */
  def update() = {
    updateTexts()
    /* Check which buttons are enabled */
    sellButton.enabled = towerClicked.isDefined && !towerSelected.isDefined
    nextRoundButton.enabled = (course.roundNo < course.rounds.length && !autoStartButton.selected && course.roundIsOver)
    targetButtons.foreach(_.enabled = !towerSelected.isDefined && towerClicked.exists(_.isInstanceOf[ShootingTower]))
    /* If the clicked tower is a shooting tower, enable target selection */
    towerClicked.getOrElse(None) match {
      case t: ShootingTower => {
        targetButtons.foreach(b => b.selected = (t.target == b.target) && targetButtons.forall(_.enabled))
       }
      case _ => targetButtons.foreach(_.selected = false)
    }
    /* Get focus back so keys can be heard and finally repaint everything */
    gameScreen.requestFocus()
    frame.repaint()
  }

  val frame: MainFrame = new MainFrame {
    title = "Tower Defense"
    resizable = false
    contents = menuScreen
    
    /* Listens declared here */
    listenTo(towerBar.mouse.clicks, towerBar.mouse.moves)
    listenTo(gameBoard.mouse.clicks, gameBoard.mouse.moves)
    listenTo(gameScreen.keys)
    targetButtons.foreach(listenTo(_))
    listenTo(pauseButton, sellButton, nextRoundButton, playButton, autoStartButton)
    levelSelectScreen.buttons.foreach(listenTo(_))
    listenTo(speedSlider)
    
    reactions += {
      case MouseClicked(source, point, _, _, _) => {
        /* Selects the tower to be bought and declares it as clicked (shows tower info)*/
        if (source == towerBar) {
          val selectionNumber = 
            (point.x / towerBar.boxWidth) +
            (point.y / towerBar.boxHeight) * towerBar.boxesHor
          try {
            towerSelected = Some(towerBar.towers(selectionNumber))
          } catch {
            case e: IndexOutOfBoundsException => towerSelected = None
          }
          towerClicked = towerSelected
          /* Places tower if possible or deselects clicked tower */
        } else if (source == gameBoard) {
          if (towerSelected.isEmpty) towerClicked = towerHovered
          towerSelected.foreach({t =>
            if (course.isLegal(mousePos, t) && Game.canAfford(t)) {
              val newTower = t.makeNew(mousePos)
              course.addTower(newTower)
              player.removeMoney(t.cost)
              towerSelected = None
              towerClicked = Some(newTower)
            }
          }) 
        }
        update()
      }
      case MouseMoved(source, point, _) => {
        /* Update position of mouse */
        if (source == gameBoard) {
          mousePos = Coord(point.x, point.y)
          /* If mouse is not on gameBoard, place it in an unseen location */
        } else mousePos = Coord(1000, 1000)
        update()
      }
      case KeyPressed(_, key, _, _) => {
        key match {
          case Key.P => Game.timer.stop()
          case Key.R => Game.timer.start()
          case Key.S => if (sellButton.enabled) towerClicked.foreach(sell(_))
          case Key.Escape => {
            towerSelected = None
            towerClicked = None
          }
          case _ => 
        }
        update()
      }
      case ButtonClicked(source) => {
          if (source == pauseButton) {
            if (source.text == "Start") player.addMoney(course.startRound)
            if (paused) Game.timer.start()
            else Game.timer.stop()
          }
          else if (source == sellButton) {
            /* Sell the currently clicked tower */
            towerClicked.foreach(sell(_))
          }
          else if (source == nextRoundButton) {
            player.addMoney(course.startRound)
            Game.timer.start()
          }
          
          else if (source == playButton) {
            this.contents = levelSelectScreen
          }
          
          else if (source == autoStartButton) {
            val b = source.asInstanceOf[ToggleButton]
            if (b.selected)
              course.autoStartRounds = true
            else 
              course.autoStartRounds = false
          }
          
          else if (levelSelectScreen.buttonHolder.contents.contains(source)) {
            val level = levelSelectScreen.buttonHolder.contents.indexOf(source)
            Game.course = Game.courses(level).makeNew
            Game.player = new Player
            this.contents = gameScreen
          }
          
          /* Target selection handling */
          else if (targetSelector.contents.contains(source)) {
            towerClicked.getOrElse(None) match {
              case t: ShootingTower => t.target = source.asInstanceOf[TargetButton].target  
              case _ =>
            }
          }
        update()
      }
      /* Speed changing */
      case ValueChanged(source) => {
        if (source == speedSlider) {
          val paused = !Game.timer.isRunning()
          val s = source.asInstanceOf[Slider]
          Game.timer.stop()
          Game.timer = new Timer(Game.tickSpeed / s.value, Game.listener)
          if (!paused) Game.timer.start()
        }
        update()
      }
    }
  }
  def top = frame
}