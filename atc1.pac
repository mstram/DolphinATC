| package |
package := Package name: 'Atc Editable-List-01'.
package paxVersion: 1;
	basicComment: 'This is an example of an animated Air Traffic Simulation written in Dolphin Smaltalk. 

The code is intended to be studied in conjunction with a seres of detailed videos showing from start to finish how a complete project such as this can be undertaken in Dolphin Smalltalk. The videos are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

Alternatively, you can just enjoy playing the game. You can open a game window, either by finding and opening  the Atc Game icon in the Samples folder, or by evaluating the following exprerssion:

AtcGameShell show "Evaluate It"

(Version 1.3)'.

package basicPackageVersion: '1.3'.


package classNames
	add: #AtcCity;
	add: #AtcGameSessionManager;
	add: #AtcGameShell;
	add: #AtcModel;
	add: #AtcPlane;
	add: #AtcPresenter;
	add: #AtcView;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Number\Dolphin Number Presenter';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models';
	add: '..\..\Users\Dell_E6430\Documents\Dolphin Smalltalk 7\Core\Contributions\Solutions Software\SSW EditableListView';
	yourself).

package!

"Class Definitions"!

Object subclass: #AtcCity
	instanceVariableNames: 'name location radius'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Object subclass: #AtcPlane
	instanceVariableNames: 'flightId position velocity color departureCity destinationCity rateOfTurn updateSpd updateTurn'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
Model subclass: #AtcModel
	instanceVariableNames: 'planes cities'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #AtcPresenter
	instanceVariableNames: 'updateProcess lastUpdateTime'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #AtcGameShell
	instanceVariableNames: 'planesPresenter planesPresenter2 airspacePresenter scorePresenter statusPresenter timer random controlPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #AtcGameSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DoubleBufferedView subclass: #AtcView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

AtcCity guid: (GUID fromString: '{F521AA21-9600-4FF8-AE19-07B5F7A01523}')!
AtcCity comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcCity is a class representing a city location in the model. AtcPlanes fly between cities.

Instance Variables:
	name		<String>
	location		<Point>
	radius		<Integer>

'!
!AtcCity categoriesForClass!Unclassified! !
!AtcCity methodsFor!

box
	^Rectangle center: self location extent: self radius*2!

color
	"
	ColorDialog showModal
	"
	^(RGB red: 196 green: 182 blue: 132)!

defaultRadius
	^50!

displayOn: aStream
	aStream display: self name!

drawOn: aCanvas 
	"
	ColorDialog showModal
	"
	| state |
	state := aCanvas save.
"
	self radius * 2 to: 500
		by: 150
		do: 
			[:n | 
			| ringBox |
			ringBox := Rectangle center: self location extent: n.
			ringBox := ringBox expandBy: (Time now asMilliseconds / 4 % 1500 //20).
			aCanvas brush: Brush transparent.
			aCanvas pen: (Pen 
						withStyle: PS_DOT
						width: 1
						color: (RGB 
								red: 224
								green: 224
								blue: 224)).
			aCanvas ellipse: ringBox].
"
	aCanvas
		brush: (Brush color: self color);
		pen: (Pen color: self color);
		ellipse: self box;
		font: (Font name: 'Arial' pixelSize: 10);
		setTextAlign: TA_CENTER;
		backgroundMode: TRANSPARENT;
		text: self name at: self location - (0 @ 6).
	aCanvas restore: state!

headingFrom: anAtcCity 
	| vector arcTan |
	vector := self location - anAtcCity location.
	vector x = 0 ifTrue: [^vector y > 0 ifTrue: [180] ifFalse: [0]].
	arcTan := (vector y / vector x) arcTan radiansToDegrees.
	(vector x > 0 and: [vector y < 0]) ifTrue: [^arcTan + 360 + 90].
	vector x < 0 ifTrue: [^arcTan + 180 + 90].

" mke temp hack"
   " ^ 180.0 "
	^arcTan +  90 !

location
	^location!

name
	^name!

radius
	radius ifNil: [radius := self defaultRadius].
	^radius!

setName: aStringName location: aPointLocation radius: anIntegerRadius 
	name := aStringName.
	location := aPointLocation.
	radius := anIntegerRadius! !
!AtcCity categoriesFor: #box!private! !
!AtcCity categoriesFor: #color!public! !
!AtcCity categoriesFor: #defaultRadius!private! !
!AtcCity categoriesFor: #displayOn:!private! !
!AtcCity categoriesFor: #drawOn:!public! !
!AtcCity categoriesFor: #headingFrom:!private! !
!AtcCity categoriesFor: #location!public! !
!AtcCity categoriesFor: #name!public! !
!AtcCity categoriesFor: #radius!private! !
!AtcCity categoriesFor: #setName:location:radius:!initializing!private! !

!AtcCity class methodsFor!

athens
	^self name: 'Athens' location: 480 @ 490 radius: 40!

frankfurt
	^self name: 'Frankfurt' location: 650@250 radius: 25!

london
	^self name: 'London' location: 250@50 radius: 50!

madrid
	^self name: 'Madrid' location: 50@480 radius: 30!

name: aStringName location: aPointLocation radius: anIntegerRadius
	^super new setName: aStringName location: aPointLocation radius: anIntegerRadius!

paris
	"^self name: 'Paris' location: 350@210 radius: 35"

            ^self name: 'CYYZ' location: 300@400 radius: 35! !
!AtcCity class categoriesFor: #athens!public! !
!AtcCity class categoriesFor: #frankfurt!public! !
!AtcCity class categoriesFor: #london!public! !
!AtcCity class categoriesFor: #madrid!public! !
!AtcCity class categoriesFor: #name:location:radius:!public! !
!AtcCity class categoriesFor: #paris!public! !

AtcPlane guid: (GUID fromString: '{6613E23F-6C97-44F1-BF97-19E2BD7300AC}')!
AtcPlane comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcPlane is a class representing a plane flying in the game model. AtcPlanes fly between AtcCitys.

Instance Variables:
	flightId		<String>
	position		<Point3D>
	velocity		<Point3D>
	color			<Color>
	departureCity	<AtcCity>
	destinationCity	<AtcCity>
	rateOfTurn		<Number>

'!
!AtcPlane categoriesForClass!Unclassified! !
!AtcPlane methodsFor!

arrivalDetectInModel: anAtcModel 
	(self position asPoint - self destinationCity location) r < self destinationCity radius 
		ifTrue: 
			[ anAtcModel trigger: #arrivalOf:at: with: self with: self destinationCity.
			
                    " Transcript show: self flightId ;show: '  ARRIVED ' ; show: self departureCity name; show: ' at ' ; show: Time new displayString. "

                  "anAtcModel destroyPlane: self"

                    ]!

box
	^Rectangle center: self position asPoint rounded extent: self boxExtent!

boxExtent
	^25@25!

collisionColor
	^Color red!

collisionDetectInModel: anAtcModel 
	| minDistance planesToTest closestPlane |
	closestPlane := nil.
	planesToTest := anAtcModel planes copyWithout: self.

        "Transcript cr; cr; show:' ------   '; show: self flightId ; show:'-----------------'; cr."

	minDistance := planesToTest inject: Float fmax
				into: 
					[:currentMin :each | 
					| distance |
					distance := (self position - each position) r.

                                     "   Transcript cr; 
							show: (each flightId) 
							; show: '  currentMin:'  ; show: currentMin displayString
                                                       ; show: '  distance:'  ; show: distance displayString.
                                        "
					distance < currentMin 
						ifTrue: 
							[closestPlane := each.
							distance]
						ifFalse: [currentMin]].

	minDistance > self collisionWarningDistance ifTrue: [^self color: self defaultColor].

	minDistance > self collisionDistance ifTrue: [^self color: self collisionColor].

	anAtcModel
		destroyPlane: self;
		destroyPlane: closestPlane.
	anAtcModel
		trigger: #planesDestroyed: with: (Array with: self with: closestPlane).

       " Transcript cr; cr; show:' ===========================================';cr. "
!

collisionDistance
	^20!

collisionWarningDistance
	^100!

color
	color ifNil: [color := self defaultColor].
	^color!

color: anIndexedColor 
	color := anIndexedColor!

defaultColor
	^Color darkGreen!

departureCity
	^departureCity!

departureCity: anAtcCity 
	departureCity := anAtcCity!

destinationCity
	^destinationCity!

destinationCity: anAtcCity 
	destinationCity := anAtcCity!

drawOn: aCanvas 
	| flText idText hdtText spdText textExtent vector velocity2D state |

	state := aCanvas save.
	aCanvas pen: Pen black.
	velocity2D := self velocity asPoint.

	velocity2D r > 0 
		ifTrue: 
			[vector := (velocity2D * self velocityVectorLength) rounded.
			aCanvas moveTo: self box center.
			aCanvas lineTo: self box center + vector.

                    "Transcript show: 'vector:  ';show: vector displayString ."
                  ].

              
	aCanvas setBkMode: TRANSPARENT.
	aCanvas font: (Font name: 'Arial' pointSize: 8).
	aCanvas fillRectangle: self box color: self color.

	"aCanvas text: self flightId at: self box bottomRight."

         " idText := 'iD', self flightId."
        idText :=  self flightId.
	textExtent := aCanvas textExtent:idText. 
	aCanvas text: idText  at: self box bottomLeft -  textExtent.
	"aCanvas text: idText  at: ((self box bottom) + 200) ."


	flText := 'FL' , self flightLevel displayString.
	textExtent := aCanvas textExtent: flText.
	aCanvas text: flText at: self box topLeft - textExtent.

	hdtText  := 'h:' , self heading displayString.
	textExtent := aCanvas textExtent: hdtText .
	aCanvas text: hdtText  at: self box topRight. 

	spdText  := 's:' , self speed displayString.
	textExtent := aCanvas textExtent: spdText .
	aCanvas text: spdText  at: self box bottomRight. 


	aCanvas restore: state


!

flightId
	"Answer the value of the receiver's 'flightId' instance variable."

	^flightId!

flightId: aString 
	flightId := aString.
	rateOfTurn := 0!

flightLevel
	^self position asPoint3D z // 100!

heading
	| vector arcTan |
	vector := self velocity.
	vector x = 0 ifTrue: [^vector y > 0 ifTrue: [180] ifFalse: [0]].
	arcTan := (vector y / vector x) arcTan radiansToDegrees.
	(vector x > 0 and: [vector y < 0]) ifTrue: [^arcTan+360 + 90].
	(vector x < 0) ifTrue: [^arcTan+180 + 90].
	^(arcTan + 90) asInteger!

heading: headingInDegrees speed: speed 
	|  vector |
	vector := (headingInDegrees - 90) degreesToRadians cos 
				@ (headingInDegrees - 90) degreesToRadians sin.
	vector := vector * speed / vector r.
	self velocity: vector!

height: anInteger 
	self position: position asPoint @ anInteger!

isNearMiss
	^self color = self collisionColor!

position
	"Answer the value of the receiver's 'position' instance variable."

	^position!

position: anObject
	"Set the value of the receiver's 'position' instance variable to the argument."

	position := anObject!

printOn: aStream 
	super printOn: aStream.
	aStream

	nextPut: $(;
		nextPutAll: self flightId; nextPutAll: '  ' ;
                nextPutAll: ' x:' ;
                nextPutAll: (self position x asInteger ) displayString ;
                nextPutAll: ' y:';
                nextPutAll: (self position y asInteger ) displayString ;
                nextPutAll: ' z:' ;
                nextPutAll: (self position z asInteger ) displayString ;
	
		nextPutAll: '  spd:';nextPutAll: self speed asInteger displayString;
                nextPutAll: ' hdg:';nextPutAll: self heading  asInteger displayString;
                nextPutAll: ' alt:';nextPutAll: self alt asInteger displayString;

	nextPut: $)!

rateOfTurn
	^rateOfTurn!

speed
	^self velocity r asInteger!

stop
	self velocity: Point3D zero!

turnAmount
	^10!

turnLeft
	rateOfTurn := rateOfTurn - self turnAmount!

turnRight
	rateOfTurn := rateOfTurn+self turnAmount!

updateForTimeInterval: timeInterval inModel: anAtcModel 
	| newPosition newHeading  v ti ups pos|

       pos := self position.
       v  :=  self velocity.
       ti  :=  timeInterval.
       ups :=   self updateSpd.

	newPosition := self position + (self velocity * timeInterval * (self updateSpd) ). 

	"newPosition :=  pos + v  * ( ti  * ups)."

	self position: newPosition.
	newHeading := self heading+(self rateOfTurn * timeInterval * (self updateTurn) ).
	self heading: newHeading speed: self speed.
	self collisionDetectInModel: anAtcModel.
	self arrivalDetectInModel: anAtcModel

!

velocity
	"Answer the value of the receiver's 'velocity' instance variable."

	^velocity!

velocity: anObject
	"Set the value of the receiver's 'velocity' instance variable to the argument."

	velocity := anObject!

velocityVectorLength
	"^0.075"
	 ^0.900 ! !
!AtcPlane categoriesFor: #arrivalDetectInModel:!private! !
!AtcPlane categoriesFor: #box!public! !
!AtcPlane categoriesFor: #boxExtent!private! !
!AtcPlane categoriesFor: #collisionColor!private! !
!AtcPlane categoriesFor: #collisionDetectInModel:!private! !
!AtcPlane categoriesFor: #collisionDistance!private! !
!AtcPlane categoriesFor: #collisionWarningDistance!private! !
!AtcPlane categoriesFor: #color!public! !
!AtcPlane categoriesFor: #color:!public! !
!AtcPlane categoriesFor: #defaultColor!private! !
!AtcPlane categoriesFor: #departureCity!public! !
!AtcPlane categoriesFor: #departureCity:!public! !
!AtcPlane categoriesFor: #destinationCity!public! !
!AtcPlane categoriesFor: #destinationCity:!public! !
!AtcPlane categoriesFor: #drawOn:!private! !
!AtcPlane categoriesFor: #flightId!accessing!public! !
!AtcPlane categoriesFor: #flightId:!private! !
!AtcPlane categoriesFor: #flightLevel!public! !
!AtcPlane categoriesFor: #heading!public! !
!AtcPlane categoriesFor: #heading:speed:!public! !
!AtcPlane categoriesFor: #height:!public! !
!AtcPlane categoriesFor: #isNearMiss!public! !
!AtcPlane categoriesFor: #position!accessing!public! !
!AtcPlane categoriesFor: #position:!accessing!public! !
!AtcPlane categoriesFor: #printOn:!private! !
!AtcPlane categoriesFor: #rateOfTurn!public! !
!AtcPlane categoriesFor: #speed!public! !
!AtcPlane categoriesFor: #stop!public! !
!AtcPlane categoriesFor: #turnAmount!private! !
!AtcPlane categoriesFor: #turnLeft!public! !
!AtcPlane categoriesFor: #turnRight!public! !
!AtcPlane categoriesFor: #updateForTimeInterval:inModel:!private! !
!AtcPlane categoriesFor: #velocity!accessing!public! !
!AtcPlane categoriesFor: #velocity:!accessing!public! !
!AtcPlane categoriesFor: #velocityVectorLength!private! !

!AtcPlane class methodsFor!

flightId: flightId from: departureCity to: destinationCity speed: speed height: height 
	| plane |
	plane := self 
				flightId: flightId
				position: departureCity location
				velocity: Point3D zero.
	plane height: height.
	plane heading: (destinationCity headingFrom: departureCity) speed: speed.
   "mike temp hack"
       "plane heading: 180."
	plane
		departureCity: departureCity;
		destinationCity: destinationCity.
         plane initialize.
	^plane!

flightId: aFlightIdString position: aPoint3DPosition velocity: aPoint3DVelocity 
	^(self new)
		flightId: aFlightIdString;
		position: aPoint3DPosition;
		velocity: aPoint3DVelocity;
		yourself! !
!AtcPlane class categoriesFor: #flightId:from:to:speed:height:!public! !
!AtcPlane class categoriesFor: #flightId:position:velocity:!public! !

AtcModel guid: (GUID fromString: '{0DE51E27-ED11-43A8-8D24-B78D1BB33C4D}')!
AtcModel comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcModel is a model class reresenting the airspace in which AtcPlanes fly between AtcCitys.

Instance Variables:
	planes		<ListModel of AtcPlanes>
	cities		<ListModel of AtcCities>

'!
!AtcModel categoriesForClass!Unclassified! !
!AtcModel methodsFor!

addPlane: anAtcPlane 
"self halt."
	planes add: anAtcPlane.
!

cities
	^cities!

destroyPlane: anAtcPlane 
	self planes remove: anAtcPlane ifAbsent: []!

drawOn: aCanvas 
	self cities do: [:each | each drawOn: aCanvas].
	self planes do: [:each | each drawOn: aCanvas].
!

initialize
	super initialize.
	planes := ListModel on: OrderedCollection new.
	cities := ListModel on: OrderedCollection new.
	self initializeCities.
        Transcript show:'AtcModel : initialize'.!

initializeCities
	cities
		add: AtcCity london;
		"add: AtcCity paris"

		add: AtcCity  cnc3;
		add: AtcCity  cyyz;

		add: AtcCity frankfurt;

		"add: AtcCity madrid;"

		add: AtcCity cykf

		"add: AtcCity athens"!

planes
	^planes!

setCities: aListModel 
	cities := aListModel! !
!AtcModel categoriesFor: #addPlane:!public! !
!AtcModel categoriesFor: #cities!public! !
!AtcModel categoriesFor: #destroyPlane:!public! !
!AtcModel categoriesFor: #drawOn:!private! !
!AtcModel categoriesFor: #initialize!initializing!private! !
!AtcModel categoriesFor: #initializeCities!private! !
!AtcModel categoriesFor: #planes!public! !
!AtcModel categoriesFor: #setCities:!public! !

!AtcModel class methodsFor!

stbConvertFrom: anSTBClassFormat 
	"Private - Version 1 adds cities instance variable."

	^
	[:data | 
	| newInstance |
	anSTBClassFormat version < 2
		ifTrue: 
			[newInstance := self basicNew.
			1 to: data size do: [:i | newInstance instVarAt: i put: (data at: i)].
			newInstance setCities: (ListModel on: OrderedCollection new)].
	newInstance]!

stbVersion
	"Version 1 adds cities instance variable"

	^2! !
!AtcModel class categoriesFor: #stbConvertFrom:!binary filing!private! !
!AtcModel class categoriesFor: #stbVersion!private! !

AtcPresenter guid: (GUID fromString: '{894F980F-1763-4E2E-9B2E-7531AF83EA1C}')!
AtcPresenter comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcPresenter is a presenter that forms the heart of the animated airspace dislplay. AtcPresenter links with AtcModel and AtcView to form an MVP triad.

Instance Variables:
	updateProcess		<Process>
	lastUpdateTime		<Number>

'!
!AtcPresenter categoriesForClass!Unclassified! !
!AtcPresenter methodsFor!

onViewClosed
	self stopUpdateProcess.
	super onViewClosed!

onViewOpened
        Transcript show: '(AtcPresenter) onViewOpened'.
	super onViewOpened.
	self startUpdateProcess. !

startUpdateProcess
	self stopUpdateProcess.
	lastUpdateTime := Time now asMilliseconds.
	updateProcess := [
			[self updateGame.
			self view repaint.
			Processor sleep: 50] repeat] 
				forkAt: Processor userBackgroundPriority!

stopUpdateProcess
	updateProcess ifNil: [^self].
	updateProcess terminate.
	updateProcess := nil!

updateGame
	| currentTime |
	currentTime := Time now asMilliseconds.
	self model planes copy do: [:each | each updateForTimeInterval: currentTime-lastUpdateTime inModel: self model].
	lastUpdateTime := currentTime.
	self trigger: #gameUpdated! !
!AtcPresenter categoriesFor: #onViewClosed!private! !
!AtcPresenter categoriesFor: #onViewOpened!private! !
!AtcPresenter categoriesFor: #startUpdateProcess!public! !
!AtcPresenter categoriesFor: #stopUpdateProcess!public! !
!AtcPresenter categoriesFor: #updateGame!private! !

!AtcPresenter class methodsFor!

defaultModel
	^AtcModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AtcView)  98 14 0 0 98 2 8 1409286144 1 416 525126 1 ##(Smalltalk.AtcModel)  0 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 530 202 208 98 5 459526 ##(Smalltalk.AtcCity)  8 'London' 328198 ##(Smalltalk.Point)  101 101 101 706 8 'Paris' 754 701 421 101 706 8 'Frankfurt' 754 1301 501 101 706 8 'Madrid' 754 101 961 101 706 8 'Athens' 754 961 981 101 0 608 786694 ##(Smalltalk.IndexedColor)  33554471 0 5 0 0 0 416 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 1 0 16 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 754 3047 21 754 751 591 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 243 5 0 0 10 0 0 0 106 7 0 0 49 1 0 0] 98 0 754 193 193 0 27 )! !
!AtcPresenter class categoriesFor: #defaultModel!public! !
!AtcPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

AtcGameShell guid: (GUID fromString: '{1F0ADDD3-3507-457F-A0D2-5FECBF48DBA0}')!
AtcGameShell comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcGameShell is the top level shell window that controls the game. 

Instance Variables:
	planesPresenter	<ListPresenter of AtcPlanes>
	airspacePresenter	<AtcPresenter>
	scorePresenter		<NumberPresenter>
	statusPresenter		<TextPresenter>
	timer				<Integer>
	random			<Random>



'!
!AtcGameShell categoriesForClass!Unclassified! !
!AtcGameShell methodsFor!

about
	"Pop up a little helpful info. about this sample program."

	self class about!

createComponents
	"Private - Create the presenters contained by the receiver"

       "planesPresenter2 := self add: ListPresenter new name: 'planes2'.    "

	super createComponents.
	
      "  planesPresenter     := self add: ListPresenter new name: 'planes'. "

 "mike replaced planes Listpresenter with Editable ListView"
        controlPresenter := self add:  ListPresenter new name: 'control'.
     
	


      airspacePresenter  := self add: AtcPresenter new name: 'airspace'.
	scorePresenter       := self add: NumberPresenter new name: 'score'.
	statusPresenter     := self add: TextPresenter new name: 'status'.

    !

createSchematicWiring
	"Private - Create the event handlers observed by the receiver"

	self 
		when: #timerTick:
		send: #onTimerTick:
		to: self.
	airspacePresenter
		when: #planeClicked:
			send: #onPlaneClicked:
			to: self;
		when: #gameUpdated
			send: #onGameUpdated
			to: self!

isGameRunning
	^timer notNil!

londonToAthens
	self newRandomFlightFrom: AtcCity london to: AtcCity athens!

londonToFrankfurt
	self newRandomFlightFrom: AtcCity london to: AtcCity frankfurt!

londonToMadrid
	self newRandomFlightFrom: AtcCity london to: AtcCity madrid!

londonToParis
	"self newRandomFlightFrom: AtcCity london to: AtcCity paris "

" mike temp for debugging "
" |flight |
^ flight :=
"
^self newRandomFlightFrom: AtcCity london to: AtcCity paris!

model: anAtcModel 
	super model: anAtcModel.
	"planesPresenter model: anAtcModel planes."

       " planesPresenter2 model: anAtcModel planes."
"mke  - for issuing commands "
        controlPresenter model: anAtcModel planes.

	airspacePresenter model: anAtcModel.
	anAtcModel
		when: #arrivalOf:at:
			send: #onPlaneArrived
			to: self;
		when: #planesDestroyed:
			send: #onPlanesDestroyed
			to: self!

newRandomFlight
	| departure destination cities |
	cities := self model cities copy.
	departure := cities at: (self random next * cities size) ceiling.
	cities remove: departure.
	destination := cities at: (self random next * cities size) ceiling.
	^self newRandomFlightFrom: departure to: destination!

newRandomFlightFrom: departure to: destination 
	| airlineCodes airlinePrefix flightNo flightId speed plane |
	airlineCodes := #(AF AZ #BA EI FI #IB LH OS SN TK).
	airlinePrefix := airlineCodes at: (self random next * airlineCodes size) ceiling.
	flightNo := (self random next * 800) ceiling + 100.
	flightId := airlinePrefix asString , flightNo displayString.

"self halt."
 
	speed := (self random next  * 30) ceiling + 30.
	plane := AtcPlane 
				flightId: flightId
				from: departure
				to: destination
				speed: speed 
			        "alt: 9000."
  	                       height: 9000.
" mike hacK"
      " plane heading: 180.0. "

"self halt."

self model addPlane: plane.
" mike added  for debugging"
^plane.!

onGameUpdated
	| nearMisses |
	self isGameRunning ifFalse: [^self].
	nearMisses := self model planes select: [:each | each isNearMiss].
	nearMisses notEmpty 
		ifTrue: [self status: 'WARNING' color: Color red]
		ifFalse: [self status: '' color: Color darkGreen]!

onPlaneArrived
	[self score: self score+self planeArrivedScore] postToInputQueue.
        self status: 'ARRIVAL' , 'abc' color: Color red.!

onPlaneClicked: aPlaneOrNil

" ifTrue:"
     "   aPlaneOrNil  ifNotNil:  [
         Transcript show: '(AtcGameShell) >>onPlaneClicked';show:aPlaneOrNil  flightId .
        }.
"

    " mike - this selects  the  planes entry in the planes list when a plane is clicked in the AtcView (graphics view)  "

        controlPresenter selectionOrNil: aPlaneOrNil

"	planesPresenter selectionOrNil: aPlaneOrNil "!

onPlanesDestroyed
	[self score: self score - self planeDestroyedScore] postToInputQueue!

onTimerTick: timerId 
	self newRandomFlight!

onViewOpened
	super onViewOpened.
	self resetGame!

planeArrivedScore
	^100!

planeDestroyedScore
	^200!

planeReleaseInterval
	^5000!

queryCommand: aCommandQuery 
	"Private - Enter details about a potential command for the receiver 
	into the <CommandQuery> argument."

	| selector |
	selector := aCommandQuery commandSymbol.

	(#(#turnLeft #turnRight) identityIncludes: selector) 
		ifTrue: 
			[aCommandQuery isEnabled: self selectionOrNil notNil.
			^true].
	^super queryCommand: aCommandQuery!

random
	random ifNil: [random := Random new].
	^random!

resetGame
	self stopGame.
	self model: self class defaultModel.
	self score: 0.
	self status: '' color: Color green.
!

score
	^scorePresenter value!

score: newScore 
	scorePresenter value: newScore.
	scorePresenter view forecolor: (newScore >= 0 ifTrue: [Color darkGreen] ifFalse: [Color darkRed]).
	newScore < 0 ifTrue: [^self youLose].
	newScore >= self winningScore ifTrue: [^self youWin]!

selectionOrNil
	"^planesPresenter selectionOrNil"

          ^controlPresenter selectionOrNil

!

startGame
	self resetGame.
	self view setTimer: (timer := 1) interval: self planeReleaseInterval.
	self newRandomFlight!

status: aString color: aColor 
	statusPresenter value: aString.
	aColor = statusPresenter view forecolor ifFalse: [statusPresenter view forecolor: aColor]!

stopGame
	self model planes do: [:each | each stop].
	timer 
		ifNotNil: 
			[:value | 
			self view killTimer: value.
			timer := nil]!

turnLeft
	self selectionOrNil turnLeft!

turnRight
	self selectionOrNil turnRight!

winningScore
	^2000!

youLose
	Sound errorBeep.
	self stopGame.
	self status: 'YOU LOSE'  color: Color darkRed
!

youWin
	Sound informationBeep.
	self stopGame.
	self status: 'YOU WIN'  color: Color darkGreen
! !
!AtcGameShell categoriesFor: #about!commands!public! !
!AtcGameShell categoriesFor: #createComponents!initializing!private! !
!AtcGameShell categoriesFor: #createSchematicWiring!initializing!private! !
!AtcGameShell categoriesFor: #isGameRunning!public! !
!AtcGameShell categoriesFor: #londonToAthens!commands!public! !
!AtcGameShell categoriesFor: #londonToFrankfurt!commands!public! !
!AtcGameShell categoriesFor: #londonToMadrid!commands!public! !
!AtcGameShell categoriesFor: #londonToParis!commands!public! !
!AtcGameShell categoriesFor: #model:!private! !
!AtcGameShell categoriesFor: #newRandomFlight!public! !
!AtcGameShell categoriesFor: #newRandomFlightFrom:to:!private! !
!AtcGameShell categoriesFor: #onGameUpdated!private! !
!AtcGameShell categoriesFor: #onPlaneArrived!event handling!private! !
!AtcGameShell categoriesFor: #onPlaneClicked:!event handling!private! !
!AtcGameShell categoriesFor: #onPlanesDestroyed!event handling!private! !
!AtcGameShell categoriesFor: #onTimerTick:!private! !
!AtcGameShell categoriesFor: #onViewOpened!private! !
!AtcGameShell categoriesFor: #planeArrivedScore!constants!private! !
!AtcGameShell categoriesFor: #planeDestroyedScore!constants!private! !
!AtcGameShell categoriesFor: #planeReleaseInterval!constants!private! !
!AtcGameShell categoriesFor: #queryCommand:!commands!private! !
!AtcGameShell categoriesFor: #random!helpers!private! !
!AtcGameShell categoriesFor: #resetGame!commands!public! !
!AtcGameShell categoriesFor: #score!public! !
!AtcGameShell categoriesFor: #score:!accessing!public! !
!AtcGameShell categoriesFor: #selectionOrNil!private!selection! !
!AtcGameShell categoriesFor: #startGame!commands!public! !
!AtcGameShell categoriesFor: #status:color:!private! !
!AtcGameShell categoriesFor: #stopGame!commands!public! !
!AtcGameShell categoriesFor: #turnLeft!commands!public! !
!AtcGameShell categoriesFor: #turnRight!commands!public! !
!AtcGameShell categoriesFor: #winningScore!constants!private! !
!AtcGameShell categoriesFor: #youLose!helpers!private! !
!AtcGameShell categoriesFor: #youWin!helpers!private! !

!AtcGameShell class methodsFor!

about
	"Private - Pop up a little helpful info. about this sample program."

	(MessageBox new)
		caption: ('About <1d>' expandMacrosWith: self);
		text: self aboutTemplate;
		open!

aboutTemplate
	"Private - Answer the text to be used in the receiver's about box"

	^'Dolphin Smalltalk Air Traffic Control Game

Steer the planes to avoid collisions. Using the mouse, select a plane in danger and
type Ctrl+L or Ctrl+R to initiate turns left or right. Reach 2000pts to win or below 
zero to lose.

DISCLAIMER: This software is freely provided purely as an educational tool and as such it
is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'!

defaultModel
	^AtcModel new!

displayOn: aStream
	"Append, to aStream, a String whose characters are a representation of the receiver as a user
	would want to see it."

	aStream nextPutAll: 'Atc Game'!

initialize
	"Private - Initialize the receiver's class variables
		self initialize
	"

	Smalltalk developmentSystem addSamplesFolderIconFor: self description: self displayString!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy) 8 ##(Smalltalk.ShellView) 98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 328198 ##(Smalltalk.Point) 2701 1501 551 0 0 0 416 0 234 256 98 0 0 461638 4 ##(Smalltalk.MenuBar) 0 16 98 4 265030 4 ##(Smalltalk.Menu) 0 16 98 5 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) 8 #startGame 8 '&Start Game' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem) 4097 690 1 722 8 #resetGame 8 '&Reset Game' 1 1 0 0 0 786 4097 690 1 722 8 #exit 8 '&Quit' 17639 1 0 0 0 8 '&Game' 0 134217729 0 0 20527 0 0 642 0 16 98 6 690 1 722 8 #newRandomFlight 8 '&New Random Flight' 9373 1 0 0 0 786 4097 690 1 722 8 #londonToParis 8 'London->&Paris' 1 1 0 0 0 690 1 722 8 #londonToFrankfurt 8 'London->&Frankfurt' 1 1 0 0 0 690 1 722 8 #londonToAthens 8 'London->&Athens' 1 1 0 0 0 690 1 722 8 #londonToMadrid 8 'London->&Madrid' 1 1 0 0 0 8 '&Flight' 0 134217729 0 0 20539 0 0 642 0 16 98 2 690 1 722 8 #turnLeft 8 'Turn Left' 9369 1 0 0 0 690 1 722 8 #turnRight 8 'Turn Right' 9381 1 0 0 0 8 '&Course' 0 134217729 0 0 20545 0 0 690 1 722 8 #about 8 '&About' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence) 202 208 98 3 721670 ##(Smalltalk.MessageSend) 8 #createAt:extent: 98 2 530 2731 21 530 2701 1501 416 1682 8 #text: 98 1 8 'An Air Traffic Simulation for Dolphin Smalltalk' 416 1682 8 #updateMenuBar 576 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 5 0 0 10 0 0 0 155 10 0 0 248 2 0 0] 98 2 410 8 ##(Smalltalk.ContainerView) 98 15 0 416 98 2 8 1140850688 131073 1936 0 196934 1 ##(Smalltalk.RGB) 30780331 0 7 0 0 0 1936 788230 ##(Smalltalk.BorderLayout) 1 1 0 0 410 8 ##(Smalltalk.StaticText) 98 16 0 1936 98 2 8 1140850944 1 2080 721990 2 ##(Smalltalk.ValueHolder) 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy) 0 2018 30780331 0 7 642 0 16 98 10 690 1 722 8 #chooseSelectionFont 8 '&Font...' 1 1 0 0 0 786 4097 690 1 722 8 #bePlain 8 '&Plain' 1 1 0 0 0 690 1 722 8 #toggleBold 8 '&Bold' 1 1 0 0 0 690 1 722 8 #toggleItalic 8 '&Italic' 1 1 0 0 0 690 1 722 8 #toggleUnderlined 8 '&Underlined' 1 1 0 0 0 786 4097 642 0 16 98 3 690 1025 722 8 #alignParagraphLeft 8 '&Left' 1 1 0 0 0 690 1025 722 8 #alignParagraphCenter 8 '&Centre' 1 1 0 0 0 690 1025 722 8 #alignParagraphRight 8 '&Right' 1 1 0 0 0 8 '&Align' 0 1 0 0 0 0 0 786 4097 690 1 722 8 #chooseSelectionColor 8 '&Colour...' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 263174 ##(Smalltalk.Font) 0 16 459014 ##(Smalltalk.LOGFONT) 8 #[245 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 2080 786694 ##(Smalltalk.IndexedColor) 33554437 8 4294902612 852486 ##(Smalltalk.NullConverter) 0 0 0 1618 202 208 98 3 1682 1712 98 2 530 1701 11 530 601 91 2080 1682 8 #contextMenu: 98 1 2240 2080 1682 1792 98 1 8 'Steer the planes to avoid collisions. Select a plane in danger and type Ctrl+L or Ctrl+R to initiate turns left or right. Reach 2000pts to win or below zero to lose.' 2080 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 82 3 0 0 5 0 0 0 126 4 0 0 50 0 0 0] 98 0 530 193 193 0 27 410 2096 98 16 0 1936 98 2 8 1140850945 1 3408 0 0 0 7 0 2962 0 16 2994 8 #[219 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 3408 2018 303 8 4294902612 3106 0 0 0 1618 202 208 98 2 1682 1712 98 2 530 11 11 530 601 91 3408 1682 1792 98 1 8 'WARNING' 3408 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 49 1 0 0 50 0 0 0] 98 0 3392 0 27 410 2096 98 16 0 1936 98 2 8 1140850945 1 3792 0 0 0 7 0 2962 0 16 2994 8 #[221 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 49 67 111 117 114 105 101 114 32 78 101 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 530 193 193 0 3792 2018 7927807 8 4294902612 852742 ##(Smalltalk.IntegerToText) 0 8 '' 0 0 1618 202 208 98 1 1682 1712 98 2 530 611 11 530 1091 91 3792 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 49 1 0 0 5 0 0 0 82 3 0 0 50 0 0 0] 98 0 3392 0 27 234 256 98 4 3792 8 'score' 3408 8 'status' 590342 ##(Smalltalk.Rectangle) 530 11 11 530 11 11 1618 202 208 98 2 1682 1712 98 2 530 5 1 530 2311 111 1936 1682 1792 98 1 8 'Info and Score' 1936 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 2 0 0 0 0 0 0 0 133 4 0 0 55 0 0 0] 98 3 3408 2080 3792 3392 0 27 410 1952 98 15 0 416 98 2 8 1140850688 131073 4496 0 0 0 7 0 0 0 4496 0 234 256 98 4 410 8 ##(Smalltalk.AtcView) 98 14 0 4496 98 2 8 1149239296 1 4592 525126 2 ##(Smalltalk.AtcModel) 0 590662 2 ##(Smalltalk.ListModel) 202 208 576 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 4706 202 208 576 0 4768 2018 14236159 0 23 0 0 0 4592 395334 3 ##(Smalltalk.Bitmap) 0 16 0 0 0 0 1 530 1871 1111 32 1618 202 208 98 1 1682 1712 98 2 530 751 11 530 1871 1111 4592 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 119 1 0 0 5 0 0 0 30 5 0 0 48 2 0 0] 98 0 3392 0 27 8 'airspace' 410 8 ##(Smalltalk.EditableListView) 98 40 0 4496 98 2 8 1140920397 1025 5056 4706 202 208 576 0 4768 482 8 4278190080 0 7 0 0 0 5056 0 8 4294902481 8 ##(Smalltalk.BasicListAbstract) 8 ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 530 65 65 0 0 202 208 98 7 1447494 14 ##(Smalltalk.EditableListViewColumn) 8 'FlightId' 181 8 #left 5216 8 ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 8 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:plane | plane flightId]' 8 #[30 105 226 0 106] 8 #flightId 5424 7 257 0 0 5056 0 1 0 0 32 0 1052998 13 ##(Smalltalk.EmbeddedTextEdit) 0 0 98 2 134349057 1 5568 2162 0 32 2208 0 0 0 5 0 0 0 5568 0 0 3106 0 8 '' 1 0 0 0 0 0 0 0 0 5330 8 'Hdg' 81 5376 459270 ##(Smalltalk.Message) 8 #displayString 98 0 5682 8 #<= 5728 5410 0 0 5442 2 1 5472 8 'doIt' 8 '[:plane | plane heading]' 8 #[30 105 226 0 106] 8 #heading 5776 7 257 0 0 5056 0 1 0 0 16 5682 8 #controlHdg: 98 1 0 5554 0 0 98 2 134349057 1 5920 2162 0 32 2208 0 482 8 4278190080 0 5 0 0 0 5920 0 0 0 1 0 0 0 0 0 0 0 0 5330 8 'Spd' 81 5376 5682 5712 5728 5682 5760 5728 5410 0 0 5442 2 1 5472 8 'doIt' 8 '[:plane | plane speed]' 8 #[30 105 226 0 106] 8 #speed 6064 7 257 0 0 5056 0 1 0 0 16 5682 8 #controlSpd: 98 1 0 5554 0 0 98 2 134349057 1 6208 2162 0 32 2208 0 482 5984 0 5 0 0 0 6208 0 0 0 1 0 0 0 0 0 0 0 0 5330 8 'Alt' 121 5376 5682 5712 5728 5682 5760 5728 5410 0 0 5442 2 1 5472 8 'doIt' 8 '[:plane | plane alt]' 8 #[30 105 226 0 106] 8 #alt 6336 7 257 0 0 5056 0 1 0 0 16 5682 8 #controlAlt: 98 1 0 5554 0 0 98 2 134349057 1 6480 2162 0 32 2208 0 482 5984 0 5 0 0 0 6480 0 0 0 1 0 0 0 0 0 0 0 0 5330 8 'Dep' 101 5376 5682 5712 98 0 5682 5760 6592 5410 0 0 5442 3 1 5408 8 'doIt' 8 '[:plane | plane dep name ]' 8 #[31 105 226 0 159 106] 8 #dep 8 #name 6624 7 257 0 0 5056 0 1 0 0 16 0 5554 0 0 98 2 134349057 1 6736 2162 0 32 2208 0 482 8 4278190080 0 5 0 0 0 6736 0 0 0 1 0 0 0 0 0 0 0 0 5330 8 'Dest' 101 5376 5682 5712 6592 5682 5760 6592 5410 0 0 5442 3 1 5408 8 'doIt' 8 '[:plane | plane dest name ]' 8 #[31 105 226 0 159 106] 8 #dest 6720 6880 7 257 0 0 5056 0 1 0 0 16 0 5554 0 0 98 2 134349057 1 6976 2162 0 32 2208 0 482 6800 0 5 0 0 0 6976 0 0 0 1 0 0 0 0 0 0 0 0 5330 8 'Info' 201 5376 5682 5712 98 0 5682 5760 7088 0 0 5056 0 1 0 0 32 0 5554 0 0 98 2 134349057 1 7120 2162 0 32 2208 0 482 8 4278190080 0 5 0 0 0 7120 0 0 0 1 0 0 0 0 0 0 0 0 8 #report 576 0 131171 0 98 4 0 0 530 1 1 0 0 202 208 576 0 0 0 3 0 0 0 0 1618 202 208 98 2 1682 1712 98 2 530 1 11 530 731 1011 5056 1682 1792 98 1 8 'FlightId' 5056 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 5 0 0 0 109 1 0 0 254 1 0 0] 98 0 3392 0 31 8 'control' 0 1618 202 208 98 2 1682 1712 98 2 530 15 131 530 2631 1151 4496 1682 1792 98 1 8 'Main Game Area' 4496 1874 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 7 0 0 0 65 0 0 0 42 5 0 0 128 2 0 0] 98 2 4592 5056 3392 0 27 3392 0 27 )!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system.
		self uninitialize
	"

	Smalltalk developmentSystem
		removeSystemFolderIconNamed: self displayString! !
!AtcGameShell class categoriesFor: #about!enquiries!private! !
!AtcGameShell class categoriesFor: #aboutTemplate!enquiries!private! !
!AtcGameShell class categoriesFor: #defaultModel!public! !
!AtcGameShell class categoriesFor: #displayOn:!displaying!public! !
!AtcGameShell class categoriesFor: #initialize!development!initializing!private! !
!AtcGameShell class categoriesFor: #resource_Default_view!public!resources-views! !
!AtcGameShell class categoriesFor: #uninitialize!development!initializing!private! !

AtcGameSessionManager guid: (GUID fromString: '{93751EE7-F4E8-4D06-8837-43AAB87DE624}')!
AtcGameSessionManager comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcSessionManager is a RuntimeSessionManager that allows the Atc Game package to be exported into a Windows executable. It is responsible for creating the AtcShellPresenter when the application starts. Note that Dolphin Professional is required to create Windows executables.

'!
!AtcGameSessionManager categoriesForClass!Unclassified! !
!AtcGameSessionManager methodsFor!

main
	"Start up the Dolphin Calculator sample application"

	self mainShellClass show! !
!AtcGameSessionManager categoriesFor: #main!operations-startup!public! !

!AtcGameSessionManager class methodsFor!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter)."

	^AtcGameShell! !
!AtcGameSessionManager class categoriesFor: #mainShellClass!constants!public! !

AtcView guid: (GUID fromString: '{C6A8563B-3D2C-4BA3-BD11-60D97103B24E}')!
AtcView comment: 'Part of the Atc Game package showing how to build an animated game in Dolphin Smalltalk. 
There is a companion series of videos showing how this game was designed. These are available at:

http://www.object-arts.com/content/videos/ProgrammingAnimation1.html

AtcView is a view that displays the contents of the animated airspace held in an AtcModel. AtcView links with AtcModel and AtcPresenter to form an MVP triad.

'!
!AtcView categoriesForClass!Unclassified! !
!AtcView methodsFor!

onLeftButtonPressed: aMouseEvent 
	| selection |
	selection := self model planes detect: [:each | each box containsPoint: aMouseEvent position]
				ifNone: [].
	self presenter trigger: #planeClicked: with: selection!

render
	| canvas |
	canvas := self canvas.
	canvas
		backcolor: self backcolor;
		erase.
	self model drawOn: canvas.
	super render!

visualObjectAtPoint: aPoint 
	^self model planes , self model cities detect: [:each | each box containsPoint: aPoint]
		ifNone: [super visualObjectAtPoint: aPoint]! !
!AtcView categoriesFor: #onLeftButtonPressed:!private! !
!AtcView categoriesFor: #render!public! !
!AtcView categoriesFor: #visualObjectAtPoint:!public! !

!AtcView class methodsFor!

defaultModel
	^AtcModel new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AtcView)  98 14 0 0 98 2 8 1409286144 1 416 525126 1 ##(Smalltalk.AtcModel)  0 590662 2 ##(Smalltalk.ListModel)  202 208 98 0 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 530 202 208 98 5 459526 ##(Smalltalk.AtcCity)  8 'London' 328198 ##(Smalltalk.Point)  101 101 101 706 8 'Paris' 754 701 421 101 706 8 'Frankfurt' 754 1301 501 101 706 8 'Madrid' 754 101 961 101 706 8 'Athens' 754 961 981 101 0 608 786694 ##(Smalltalk.IndexedColor)  33554471 0 5 0 0 0 416 395334 3 ##(Smalltalk.Bitmap)  0 16 0 0 0 0 1 0 16 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 754 3047 21 754 751 591 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 243 5 0 0 10 0 0 0 106 7 0 0 49 1 0 0] 98 0 754 193 193 0 27 )! !
!AtcView class categoriesFor: #defaultModel!public! !
!AtcView class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

