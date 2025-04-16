"Dreamland" by mellyeliu

roomCount is a number that varies. roomCount is 0.
songX is a number that varies. songX is 0.
bookDrawn is a number that varies. bookDrawn is 0.
bookWrite is a number that varies. bookWrite is 0.
emmaName is a number that varies. emmaName is 0.
drawingHint is a number that varies. drawingHint is 0.
suitcaseOpen is a number that varies. suitcaseOpen is 0.
helpInstructions is a number that varies. helpInstructions is 0.

Helping is an action applying to nothing. 
Understand "help" as helping.

Abouting is an action applying to nothing. 
Understand "about" as abouting.

Carry out helping:
	say "[line break]
	[italic type]Here are some instructions to get you started:[line break]	• [bold type]look[roman type] to observe your surroundings[line break]
• [bold type]examine item[roman type] or [bold type]x item[roman type] to look at any object[line break]
    • [bold type]take item[roman type] or [bold type]take all[roman type] to pick up items[line break]
    • [bold type]go to room[roman type] to teleport to a specified room[line break]
    • [bold type]inventory[roman type] or [bold type]i[roman type] to see what you're carrying[line break]
    • [bold type]go direction[roman type] to move around (e.g., [bold type]go north[roman type]  or simply [bold type]n[roman type]  or [bold type]s[roman type])[line break]
• [bold type]https://tinyurl.com/inform-cheatsheet[roman type] for more commands.[roman type] [line break]
";

Carry out abouting:
	say "[line break][italic type]This is a little demo of an interactive fiction game about dream journals and imaginary worlds. Please be patient as we work through any bugs ૮ ․ ․ ྀིა.[line break]
";


[female childhood best friend

fantasy version of childhood
starts out in a dream of trying to save your friend in a tower.
you wake up in your bedroom.
eat candy, makes world fantastical.
dream world logic
plushieland - stuffed animal talks
toybox - crayon land where you use crayons to fix the world
falling out
tragedy
in jail for killing friend?
girl is imaginary?
girls that dream together but have never met.
multiple paths
doki doki
unreliable narrator
she hits the delete button


writing a children's book
fantasy 
create a novel
pill bottles instead of candy
inventory pill instead of candy
physical symptoms of drug withdrawal

manuscript is visible on a desk
traumatic chapter 10 times to get the content out

terminal illness due to lack of belief? how to keep her alive:
	- bits of her body start fading
	- collect various objects for cure?
	- each toy world is you granting her a wish, you sharing in belief
	- multiple endings: 
		- maladaptive daydreaming? too much absorption in the world 
		- she dies - sterile corporate present day
		- imaginary world as muse
		
toybox (doodles)
plushieland (cotton plushies)
pure diary? world of pure text (nothing appears? everything is represented by their text form OR journal that you can write in chamber of secrets style)
video game rendering world (3d greyscale)


dream: early 2000s
high school 
computer dream

endings:
	she coded it all up as fantasy escapism
	god complex ?
	imaginary friend
	she erased her own memory
	
	she did something bad (memory) - first dream
	
two friends creating world together
OR a girl and an imaginary friend creating the world together

friend could die OR go missing OR be estranged
	
]

When play begins:
say "You wake up to the ringing of your alarm. Images of a strange dream drift through your mind, but when you try to conjure them, you draw a blank.

(´｡• ᵕ •｡`) ♡
".

[After printing the banner text:
    say "[line break][italic type]What we share may be a lot like a traffic accident but we get one another. We are survivors of each other. We have been shark to one another, but also lifeboat. That counts for something. [line break]".]

Teleporting is an action applying to one visible thing. Understand "go to [any room]" as teleporting. Understand "go [any room]" as teleporting. Carry out teleporting: move the player to the noun.

Check teleporting:
	if the noun is the Treehouse:
		if the ladder is broken:
			say "The ladder is missing some rungs and isn't usable right now. You need to fix it first.";
			stop the action.

[Section 0 - Dreamland

Dreamland is a room. "It happens before you register it.
'You have to go. Get out of here.".

Every turn when the location of the player is Dreamland:
	say "The shadows are growing darker.".


Emma is a person. The description of Emma is "The girl is wearing a red checkered dress. Her hair is in knotted and face streaked with mud. Her ankle is red and swelling.". Emma is here.

Instead of asking Emma about "i love you":
	say "Emma is weeping. 'I love you, too.'";
Instead of asking Emma about something:
	If the location of the player is Dreamland:
		say "Emma won't listen to you. Tears are running down her face. 'Leave me', she chokes out. 'Run away from here'.";
	else:
		if emmaTalk is 1:
			say "Emma doesn't seem to hear you. She seems lost in a daydream.";
		else if emmaTalk is 2:
			say "Emma is too busy digging through the meadow.";
		else if emmaTalk is 3:
			say "Emma is humming to herself.";
		now emmaTalk is a random number from 1 to 3.

Rule for printing the locale description of Dreamland: stop.

Deserting is an action applying to one visible thing. Understand "leave [someone]" or "desert [someone]" as deserting.

Carry out deserting:
	If the location of the player is Dreamland:
		say "You feel like you're being sucked out of this world. You feel a whirlwind of crying, of pain, of grief, of everything crashing down on you in that moment. When the whirling stops you find yourself in your bedroom. Wake up.";
		move Emma to the Toybox;
		move the player to the Bedroom.
]

Section 1 - BEDROOM

House is a region. Bedroom is in house. Kitchen is in house.

The Bedroom is a room. "[fixed letter spacing]
.  \_/       .:'    .:'    .:'[line break]
.-=(_)=-  /\||   /\||   /\||[line break]
.  / \   //\\|  //\\|  //\\|[line break]
.       //  \\ //  \\ //  \\[line break]
.      //    \^/    \^/    \\[line break]
.      |()  ()|()  ()|()  ()|[line break]
.     &|  ||  %  ||  |  ||  |%[line break]
.  &%&--==--&%-==--%&=--%&%&=--%&[variable letter spacing]

[if roomCount is 0]Your bedroom is unadorned. The walls are chalky and the ground bare. The only decoration is a poster of The Killers. A lightbulb hangs from the ceiling. A tiny window is opened a crack, and the sound of faint static comes through. A crumpled newsprint lies in the wastebin, forgotten.[else if roomCount is 1]You step into the bedroom and are flooded with emotion. Every inch of its surface now looks as if it's been dipped in. The walls are pulsating and warm, shifting from blue to red to violet. [else if roomCount is 2]It's weird to see things in 3D now. Your room has gone back to normal. There's an odd humming coming from your computer, though. [end if]

[if roomCount is 0]In the corner your monitor blinks wearily. You must've forgotten to turn it off. [else if roomCount is 1]Lo-fi indie is playing from your computer. You notice movement on the screen.[end if] Beside you is a worn out drawer. To the north your kitchen fridge hums. To the south is a cramped closet.
[if helpInstructions is 0][paragraph break][italic type]Type [bold type]help[italic type] or [bold type]about[italic type] to get started.[roman type][end if]
".

After looking in the Bedroom:
	now helpInstructions is 1;

A window is here. It is fixed in place. The description of window is "North facing. Not a lot of light coming in.".A poster is here. It is fixed in place. A lightbulb is here. It is fixed in place. It is a device. It is switched off. A drawer is here. It is a container. It is openable. It is closed. It is locked. It is fixed in place. A journal, crayons, and glasses is inside the drawer. The glasses is wearable.
The description of drawer is "It's covered with a layer of dust and lined with faint scratches. You can make out a tiny keyhole in the upper right corner.". A keyhole is here. It is scenery. The description of keyhole is "A standard keyhole.".
The description of lightbulb is "A single lightbulb. A string dangles.".
The description of poster is "A relic of your emo era. The corners have frayed over the years. ".
The description of crayons is "A bright yellow box. On it are instructions on how to draw certain shapes: a bunny, a lion, a narwhal, and a unicorn. Good for fixing things, if you're in the right place.

[italic type]To draw something with the crayons, type: draw `object` with crayons[roman type].".
The description of glasses is "A wacky looking pair of with black frames and blue-purple swirling lenses. On the side are the words [italic type] Sentence Scrambler."
	
After opening the drawer:
	say "You pull open the drawer. A journal, a box of crayons, and a pair of glasses	 lies inside. The journal beckons to you.".

After putting on glasses:
	say "You put on the pair of glasses and the world turns all sorts of odd colours. Guess you need to change your prescription.".

A desk is here. It is a supporter. It is fixed in place. A ballpoint pen is on the desk. It is fixed in place. A computer is on the desk. A computer is a device. It is switched on. It is fixed in place. The description is "[if noun is switched off]A yellowing Windows XP that's seen better days.[else if roomCount is 0]The computer is open to a game development software. You feel unsettled, like you're being watched.[else if roomCount is 1]Your screen is being populated with a barrage of popups. The same message is written on them. 'Your dreams await. \( ^^)/'.[else if roomCount is 2]
A browser is open to an email inbox. You have several unread emails.
[end if] "


After examining the computer:
	if roomCount is 2:
		say "The browser is open to your email inbox. You have 5 unread emails. [italic type]You can select an email to read by typing a number from 1 to 5. You can type `exit` to leave.[roman type]";
		now the command prompt is "> Select an email to read: ".

After reading a command when the command prompt is "> Select an email to read: ":
	if the player's command matches "5":
		say "From: Emma - Word Search from Emma[paragraph break]Hi! I made you a word search. It's neopet themed! 
		
 . J T A M X I L L S A[line break]
H Y B Z A P F Z X R[line break]
U R E D O U B L E P[line break]
D F S E T Y W E T I[line break]
O L I L I S H A E N[line break]
R J N L O F A E U J[line break]
A F F G C I D O L S[line break]
P A I N T B R U S H[line break]
B I L L U S E N F T[line break]
J U B J U B A S S G[line break]
		";
		reject the player's command;
	else if the player's command matches "2":
		say "From: Mr. Anderson - Welcome to 5th Grade![paragraph break]Dear Student, Welcome to 5th grade! We are excited to have you. Make sure to bring your materials on the first day. Your new teacher, Mr. Anderson.";
		reject the player's command;	
	else if the player's command matches "3":
		say "From: Emma - Ideas for Plushieland[paragraph break]Hey, I had some ideas for the fictional world we're creating together! We should have flying narwhals, secret caves, and magical powers that only the bravest can use! Let me know what you think.";
		reject the player's command;
	else if the player's command matches "4":
		say "From: School Admin - Field Trip to the Science Museum[paragraph break]Parents and Guardians, We are planning a field trip to the Science Museum next month. Please submit the signed permission slip by Friday.";
		reject the player's command;
	else if the player's command matches "1":
		say "From: Art Class Reminder - Reminder: Bring Your Crayons to Art Class![paragraph break]This is a reminder to bring your crayons to art class next week. We’ll be doing a special drawing activity.";
		reject the player's command;
	else if the player's command matches "exit":
		now the command prompt is "> ";
		reject the player's command;
	else:
		say "Invalid selection. Please select an email to read by typing a number from 1 to 5";
		reject the player's command;
		now the command prompt is "> Select an email to read: ".
		
Reading email is an action applying to one number. Understand "read email [number]" as reading email.

Understand "monitor" or "laptop" as computer.
Understand "lamp" or "light" as lightbulb.
Understand "pull string" as pulling the string.
Pulling the string is an action applying to nothing.
Understand "Killers" or "Killers poster" or "wall" as poster.

Instead of pulling the string when the lightbulb is switched off:
    try switching on the lightbulb.

Instead of pulling the string when the lightbulb is switched on:
    try switching off the lightbulb.


Rule for printing the locale description of Bedroom: stop.

Carry out switching on the lightbulb:
	say "You pull the string, and the lightbulb flickers to life.";

Carry out switching off lightbulb:
	say "You pull the string, and the lightbulb goes dark.".

After switching on the lightbulb:
    stop the action.

After switching off the lightbulb:
    stop the action.

After switching on computer:
	say "The monitor lights up with a staticky pop.";
	try examining the computer.

Carry out switching off computer:
	say "The monitor shuts off. The fan goes silent.".
	
A newsprint is here. "A crumpled newsprint lies wrinkled on the floor." The description is "You straighten out the sheet of newspaper, handling it delicately. [if roomCount is 0]You can't quite make out the words. Your head is spinning.[else if roomCount is 1] The page contains two girls playing in a sandbox. The smaller of the two is labouring over a crudely formed sand sculpture. The other is carelessly sprawled across the sand, her body covered in dust. You study the small stature, the neat braids, the wrinkled clothing. They look around preschool age. [paragraph break]¯¯̿̿¯̿̿'̿̿̿̿̿̿̿'̿̿'̿̿̿̿̿'̿̿̿)͇̿̿)̿̿̿̿ '̿̿̿̿̿̿\̵͇̿̿\=(•̪̀●́)=o/̵͇̿̿/. You can't make out the caption.[end if]".

[A paper is a kind of thing. A paper has a doodle. After printing the name of a paper: say " (with a [doodle] on it)". Understand the doodle property as describing a paper.]

A thing can be examined or unexamined.
After taking something unexamined:
    say "Taken. [run paragraph on]";
    try examining the noun.
Carry out examining something:
    now the noun is examined.

[Understand "draw [doodle] on [any thing]" or "draw [doodle] in [thing]" or "doodle [doodle] on [thing]" or "doodle [doodle] in [thing]" or  "doodle [doodle] on [thing]"  or "sketch [doodle] on [thing]" or "sketch [doodle] in [thing]" as drawing. Drawing is an action applying to one doodle and one thing.]

[Report drawing:
	say "You doodle on the [noun]. Looks cute.".]

Section 2 - Closet

The Closet is south of the Bedroom. "The closet is small and cramped, with a suitcase in the corner. It's dimly lit, and you can smell a faint musty odor, as if the space hasn’t been opened in years. The walls are lined with old, forgotten coats, and the ceiling is low, making the space feel claustrophobic."

A suitcase is here. The description of the suitcase is "[if the suitcase is closed] A small black suitcase and a keypad on the side. It looks like it requires a passcode to open. [line break][line break][italic type]You can enter a passcode by typing 'open suitcase'.[roman type] [else] Inside the suitcase is a collection of random objects, a yearbook, a record player, and a narwhal.". A suitcase is locked and closed. The suitcase is a container. It is fixed in place. A yearbook, record player, and narwhal is in the suitcase. A keypad is here. It is fixed in place. The description is "A numeric keypad, a little old fashioned. The tag reads '[italic type]My favourite neopet[roman type]'.".

	
Instead of opening the suitcase:
	now the command prompt is "> This suitcase requires a passcode: ";


After reading a command when the command prompt is "> This suitcase requires a passcode: ":
	if the player's command matches "jubjub":
		now the suitcase is open;
		now the command prompt is ">";
		say "The suitcase clicks open. Inside the suitcase is a collection of random objects, a yearbook, a record player, and a narwhal";
	otherwise:
		say "The suitcase beeps. The passcode is incorrect.";
		now the command prompt is ">";
	reject the player's command.

A yearbook is a container. It is openable. The description is "[italic type][if roomCount is 0] ??? ???? ???? ???? You can't make out the text right now.[else if the player is wearing the glasses]Class of XXYZ[end if][roman type].".
A narwhal is a thing. The description is "A ragged thing. An old hurt vibrates through you. You can't place why.".
A record player is a device. It is switched off. It is fixed in place. The description is "A vintage turntable. It's loaded with a vinyl - Best of 2010s Hits.".

Section 2 - KITCHEN

The Kitchen is north of the Bedroom. "
[fixed letter spacing]
.        `'::. [line break]
.    _________H ,%%&%, [line break]
.   /\     _   \%&&%%&% [line break]
.  /  \___/^\___\%&%%&& [line break]
.  |  | ||   || |%\Y&%' [line break]
.  |  |   .-.   | ||  [line break]
.~~@._|@@_|||_@@|~||~~~~~~~~~~~~~[line break]
.     `''') )'''`[variable letter spacing]

Your kitchen has seen better days. A fridge lies in one corner. Near the door is a row of plants with yellowing leaves."

The silver key is a thing. The description is “A stainless steel key, slightly bigger than your thumb. It's wet with your saliva.” The matching key of the drawer is the silver key. The silver key is nowhere.

After unlocking the drawer with the silver key:
	say "The drawer yawns open lazily. In it are a journal, crayons and glasses.";
	now the drawer is open;

A jug is here. The description is "It's filled with water."

The plant is a thing. It is here. It is fixed in place. The description is "It looks parched.".
Understand "water [something]" or "water [something] with jug" or "pour water on [something]" as watering. Watering is an action applying to one thing.

After watering the plant:
	if the player is carrying the jug:
		say "You water the plant with the jug. The plant perks up a little.";
		now the description of the plant is "It's standing a bit taller now.";
		now the description of the jug is "An empty jug.";
	else: 
		say "You don't have anything to water it with.".

A note is here. It is fixed in place. A baking soda is here. It is fixed in place. The description is "Off-brand Great Value. Good for neutralizing odours.". A fridge is here. The fridge is fixed in place.  The description of the fridge is "You're bizarrely attached to your fridge. It's charming in its age, rickedy, humming in a contented manner. It's covered with  and a note is pinned to it." It is a container. It is openable. It is closed. The description of the note is "There's a note stuck to the fridge. It reads [if the player is not wearing the glasses]??? ??? ????????? ????. You can't make out the text[else] 'Where did you go? Come play with me in Dreamland /( ^^)/[end if].'".

After opening the fridge, say "The fridge is empty except for a tub of baking soda and a piece of candy. The candy beckons to you."

The candy is edible. The description is "A lychee sweet. It glows mysteriously."  The fridge contains candy.  

After taking the candy:
	say ""

After eating the candy:
	say "
[fixed letter spacing]
.                                _'' _[line break]
. _'' _                         (_\|/_)[line break]
.(_\|/_)  _'' _         _ ' _    (/|\)[line break]
. (/|\)  (_\|/_)'' _   (_\|/_)[line break]
.         (/|\)_\|/_)   (/|\)[line break]
.              (/|\)[variable letter spacing]

You unwrap the candy and pop it in your mouth. It tastes of a strange medley of flavours—vanilla custard, citrus, a hint of buttered toast. Delightful.

A sudden prickling rips through your throat, like you've swallowed something you shouldn't have. You double over, coughing, clutching the counter for balance. There's something lodged in your throat.

[italic type]Clang![roman type] Out comes a small piece of metal. You pick it up, bewildered. It's a small key. 

[italic type]You can press i to view your inventory.[roman type]";
	now the player is carrying the silver key;
	now roomCount is 1;


Section 3 - Toybox

Toybox is a room. "
 |                  /\   [line break]
|                 / | \ [line break]
|                / | | \ [line break]
|               /_|_|_|_[line break]
|                |     / /[line break]
|__    __    __ |    / /__    __    __[line break]
|  |__|  |__|  |.   / /|  |__|  |__|  |[line break]
|__            ____/ /___           __|[line break]
   |          / .------  )         |[line break]
   |         / /        /          |  TOYBOX[line break]
   |        / /        /           |[line break]
~~~~~~~~~~~~-----------~~~~~~~~

You open your eyes to pure whiteness. Every bit of empty space is blue lines, pink margins...

Margins! That's it. The world you're in is rendered on notebook paper. Objects are flat, paperlike, outlined in waxy lines. You look down at your hands and suppress a cry of shock. You're wearing a triangular smock of purple and your body is composed of black lines. You unfurl your fingers and watch as the lines retract and relax.

You take in the rest of your surroundings. You seem to be in a playground of sorts. There's a set of swings and a playstructure in a garish primary colours. In the distance is a forest, the paper trees swaying in the wind. One of the trees stands proudly in the center, a broken ladder propped against it." 
Toybox is a room with printed name "Toybox (Dreamland)".

[A doodlebox machine is in the Toybox. It is fixed in place and opaque. ]


A playstructure is here. It is fixed in place. Understand "playground" or "park" as playstructure. The description of the playstructure is "Warped plastics. Doesn't look safe to visit.". A swing is here. It is fixed in place. Understand "swings" as swing. The description of the swing is "A rusted thing. Doesn't look safe to swing on.". A tree is here. It is fixed in place. The description of the tree is "A knotted looking thing with a thick trunk. Leaning on it is a worn out ladder. Nestled in the branches is a winding structure, a treehouse of sorts.". A forest is here. It is scenery. The description of the forest is "A series of swaying paper trees. In the middle is a lush treehouse.". A bunny is here. It is fixed in place. A clover is here. It is fixed in place. A dandelion is here. It is fixed in place.

Drawing is an action applying to one topic and one carried thing.
Understand "draw [text] with [something]" as drawing.

Drawnothing is an action applying to one topic.
Understand "draw [text]" as drawnothing.

Carry out drawnothing:
	say "What do you want to draw with?";

Check drawing:
	if the second noun is not the crayons:
		say "You need to use crayons to draw that!" instead;
	if the player does not carry the crayons:
		say "You don't have any crayons to draw with." instead;

Carry out drawing:
	if the topic understood matches "dandelion":
		now the player is carrying the dandelion;
		say "[fixed letter spacing]		
.       wWWWw              wWWWw[line break]
. vVVVv (___) wWWWw        (___)  vVVVv[line break]
. (___)  ~Y~  (___)  vVVVv  ~Y~   (___)[line break]
.  ~Y~   \|    ~Y~   (___)   |/    ~Y~[line break]
.  \|    \|/   \| /  \~Y~/  \|    \ |/[line break]
. \\|// \\|// \\|/// \\|// \\|// \\\|///[variable letter spacing]

You use the crayons to carefully draw a bright yellow dandelion. The petals are vivid, and the stem is a bright green. The drawing looks almost alive. 
		
		The air sizzles, and with a pop, something drops in your pocket.";
	else if the topic understood matches "bunny":
		say "You doodle a cute bunny on the page. Its long ears and fluffy tail make it look adorable.";
	else if the topic understood matches "ladder":
		now the ladder is fixed;
		say "You color in the missing gaps with the silver crayon. Good as new.";
	else if the topic understood matches "on ladder":
		now the ladder is fixed;
		say "You color in the missing gaps with the silver crayon. Good as new.";
	else if the topic understood matches "unicorn":
		say "You carefully sketch a majestic unicorn with your crayons. Its flowing mane and shiny horn are drawn with great detail.";
	else:
		say "You draw [the topic understood] with your crayons. It looks decent, but it's not your best work.";

Rule for printing the locale description of Toybox: stop.

Section 3,1 - EMMA

The Treehouse is a room. "[fixed letter spacing]
.          &&& &&  & &&[line break]
.      && &\/&\|& ()|/ @, &&[line break]
.      &\/(/&/&||/& /_/)_&/_&[line break]
.   &() &\/&|()|/&\/ '%'' & ()[line break]
.  &_\_&&_\ |& |&&/&__%_/_& &&[line break]
.&&   && & &| &| /& & % ()& /&&[line break]
. ()&_---()&\&\|&&-&&--%---()~[line break]
.     &&     \|||[line break]
.             |||[line break]
.             |||[line break]
.             |||[line break]
.       , -=-~  .-^- _[variable letter spacing]

You have entered a cozy treehouse. It has a wooden floor, a small table with some board games, a hammock, and a small girl around the age of 6. There's a window with a view of the forest below.

The girl is doodling furiously. It's some sort of composition of flowers. Every now and then she pauses to wipe her wax-covered hands on her skirt. When she sees you, she cries out in delight. 

'I see you managed to fix the ladder,' she says. I was getting bored up here. What took you so long?'"

The ladder is a supporter in the Toybox. The ladder can be fixed or broken. The ladder is broken. The description of the ladder is "The ladder leans against the tree, but some of its rungs are missing."

Rule for printing the locale description of the Treehouse:
	stop.


[Carry out teleporting to the Treehouse:
	say "The ladder is missing some rungs and isn't usable right now. You need to fix it first.";
	stop the action;]

Instead of climbing the ladder when the ladder is broken:
	say "That would be dangerous."

Drawing on it with is an action applying to two things. Understand "draw on [something] with [something]" as drawing on it with.

Check drawing on the ladder with the crayons:
	if the player does not carry the crayons:
		say "You need something to draw with." instead;
	if the ladder is fixed:
		say "You have already fixed the ladder." instead.

Carry out drawing on the ladder with the crayons:
	now the ladder is fixed.	

Report drawing on the ladder with the crayons:
	say "You color in the missing gaps with the silver crayon. Good as new."
	
Carry out drawing on the ladder with the crayons:
	now the ladder is fixed.

Report drawing on the ladder with the crayons:
	say "You color in the missing gaps with the silver crayon. Good as new."
	

Instead of climbing the ladder when the ladder is fixed:
	say "You grip the edges of the ladder cautiously, and hoist yourself up. Half of you expects the scribbled lines to give way, but they hold steady. After a few minutes of climbing, you are met with a trapdoor. You push on the latch.";
move the player to the Treehouse.

After going to the Treehouse for the first time:
	say "'Hi there!' Emma says. 'I see you managed to fix the ladder. I was getting bored up here. What took you so long?'"

emmaTalk is a number that varies. emmaTalk is 1.

Emma is a person in the Treehouse. The description of Emma is "
[fixed letter spacing]
.     _          _          _         [line break]
.    //|\       //|\       //|\     [line break]
.    c_''/      c_''/      c_''/    [line break]
.   __/\__       /\__       /\     [line break]
.    /  \       /\ \       /\/\     [line break]
.   /____\     /____\     /____\   [line break]
.     /l         ll         l\       [line break]
[variable letter spacing]
[if Emma is in Treehouse]A small girl wearing a checkered dress. Her hair is in neat plaits and her socks are mismatched. She looks familiar. [else]Emma looks taller than you remember. She's fiddling with a sewing needle.[end if]". Understand "girl" or "child" or "kid" as Emma.

Instead of telling someone about something, try asking the noun about it. Instead of answering the noun that something, try asking the noun about it.

Understand "ask [someone] [text]" as asking it about.
Understand "tell [someone] [text]" as answering it that. Understand "tell [someone] that [text]" as answering it that.
Giving it about is an action applying to two things. Understand "give [someone] [something]" or "give [something] to [someone]" as giving it about.

After examining a person: say "[italic type]You can interact with [the noun] by typing 'ask [noun] about topic'. The topic should be a single word.[roman type]".


Carry out giving something about someone:
	say "Emma cries out in joy. A [the second noun]!";
Before giving narwhal to Emma:
	say "'Mr. Narkins!' she cries. 'You've returned him to me. But his wings are gone...'. She sobers up. 'That's to be continued. This is a demo, after all.'";
	end the story finally saying "You've reached the end of the demo (´｡• ᵕ •｡`) ♡ See you again soon.";

	
Before giving dandelion to Emma:
	say "
[fixed letter spacing]
.     _          _          _         [line break]
.    //|\       //|\       //|\     [line break]
.    c_''/      c_''/      c_''/    [line break]
.   __/\__       /\__       /\     [line break]
.    /  \       /\ \       /\/\     [line break]
.   /____\     /____\     /____\   [line break]
.     /l         ll         l\       [line break]
[variable letter spacing]
	
	Emma looks up at you in wonder. 'A dandelion', she whispers reverentially. 'I've never seen one before.'

	You tell her they're commonplace where you're from. 'That can't be right,' she says. 'My grandma says they went extinct long ago. They were killed, you know. For being too resilient.' She cups the fluffy white thing delicately. 'C'mon! Let's wish together.'

	You put your heads together and your breaths create a flurry of white as the spores release into the air. You stare at this strange girl, wide-eyed with awe, twirling the empty stem in her hands. You're drawn to her uncomplicated joy.

	'I can't tell you what I wished for,' she says peevishly, as if anticipating your next question. 'Cause then it won't come true! But I do want to see you again. It gets lonely here, you know'.
	
	She gazes at you in wonder. 'I've never had a friend. Will you be my friend?";
	now the command prompt is "> Befriend Emma? Y/N ";
	reject the player's command.

The crayola key is nowhere. It is fixed in place.

After reading a command when the command prompt is "> Befriend Emma? Y/N ":
	if the player's command matches "Y":
		say "Her face blooms with delight. 'YAAAAY', she cries. 'We can all be friends now. Me, you, Mr. Narkins.

		Her face darkens for a second. 'Have you seen him, by the way? He's a sea unicorn.' You feel suction around you, like you're going through a vacuum. Everything blurs.";
		now the player is carrying the crayola key;
		now roomCount is 2;
		move player to Bedroom;
	else if the player's command matches "N":
		say "Emma deflates for just a second. Then she shrugs it off, and says, 'That's okay! Just let me know if you change your mind ^_^.";
	else:
		say "That's not a valid response.";
	now the command prompt is ">";
	reject the player's command.


Instead of kissing Emma:
	say "Emma turns bright red. Whadya doing?".
Instead of asking Emma about "emma":
	now emmaName is 1;
	say "She brightens. That's me!".
Instead of asking Emma about "friends":
    	if Emma is carrying the dandelion:
		now the command prompt is "> Befriend Emma? Y/N".
Instead of asking Emma about "name":
    now emmaName is 1;
    say "Huuuh? I'm Emma, silly. Did you hit your head on the way down here?".
Instead of asking Emma about "dream":
    say "[if emmaName is 1]Emma[else]The girl[end if]is crestfallen. So it's true. You think we're only dreaming.".
Instead of asking Emma about "hi":
    say "[if emmaName is 1]Emma[else]She[end if] crushes you in a hug. You found me!".
Instead of asking Emma about "drawing":
    say "'C'mon! Let's draw together', she cries. There's something I'm trying to draw...".
Instead of asking Emma about "doodle":
    say "'C'mon! Let's draw together', she cries. There's something I'm trying to draw...".
Instead of asking Emma about "drawing":
    say "'C'mon! Let's draw together', she cries. There's something I'm trying to draw...".
Instead of asking Emma about "crayons":
    say "Her eyes go wide. 'Waahhhh! Crayons! Can I eat one?' she cries. 'Just kidding. But I wonder if you could help me out with my wish.'".
Instead of asking Emma about "wish":
    say "You feel it too, don't you? whispers [if emmaName is 1]Emma[else]the girl[end if]. This is a magic place. I'm looking for a flower that'll make things come true...".
Instead of asking Emma about "looking/find/doing/grass/stem/meadow":
    say "[if emmaName is 1]Emma[else]The girl[end if] looks solemn, 'I'm looking for something. It's a [italic type]special[roman type] plant. My grandma told me it's got special powers, and I need it to make a wish.".
Instead of asking Emma about "wish/what her wish is/wishes":
    say "Huuuh? I can't tell you! 'Cause if I did, it wouldn't come true!".
Instead of asking Emma about "treehouse":
    say "[if emmaName is 1]Emma[else]The girl[end if] smiles. I spent a while on it! Used the brown crayon and the green crayon and lots of other things... I'm trying to decorate it with flowers.'".
Instead of asking Emma about "flower":
    say "[if emmaName is 1]Emma[else]The girl[end if] brightens up. 'Yes! I'm trying to draw a specific flower... for a wish. It's yellow, and then not yellow. When it's old it becomes a puffball.'".
Instead of asking Emma about "flowers":
    say "[if emmaName is 1]Emma[else]The girl[end if] brightens up. 'Yes! I'm trying to draw a specific flower... for a wish. It's yellow, and then not yellow. When it's old it becomes a puffball.'".
Instead of asking Emma about "dandelion":
    say "[if emmaName is 1]Emma[else]The girl[end if] looks up at you in wonder. So you do know the story! Could'ya help me find one? Pretty please!".
Instead of asking Emma about "clover":
    say "[if emmaName is 1]Emma[else]The girl[end if] shakes her head. Those don't work. ".
Instead of asking Emma about "narwhal":
    say "'Mr. Narkins!' she cries.".
Instead of asking Emma about "parents":
    say "[if emmaName is 1]Emma[else]The girl[end if] goes quiet for a long time. 'I don't know who they are', she finally says.".
Instead of asking Emma about "fuck":
    say "[if emmaName is 1]Emma[else]The girl[end if] covers her ears and turns red. 'Stop it with the bad words or I'll tell on you!'".
Instead of asking Emma about something:
	if emmaTalk is 1:
		say "Emma doesn't seem to hear you. She seems lost in a daydream.";
	else if emmaTalk is 2:
		say "Emma is too busy doodling flowers.";
	else if emmaTalk is 3:
		say "Emma is humming to herself.";
	now emmaTalk is a random number from 1 to 3.
Instead of asking Emma about "world", say "She looks at you quizzickly. 'Whatdya mean? You don't remember?'".
Instead of asking Emma about "toybox", say "She looks at you quizzickly. 'Whatdya mean? You don't remember?'".

Section 4 - Plushieland

Plushieland is a room. "You wake up in world of cotton. Everything you see is rendered in a shade of pastel - the turgid river, the sky, the meadows in front of you. You reach out a hand, tentatively. The cotton is soft.

Before you are a medley of anthropomorphic plushies. You spot a jubjub, hedgehog, and even a djungelskog. A girl around the age of seven is playing with the stuffed animals.

To the north you see a yellow schoolhouse. To the south there is a vibrant meadow."

A river is in Plushieland. "A turgid river, made of fluffy blue transparent cotton." It is scenery. A sky is in Plushieland. "A fluffy blue sky. The clouds are soft to touch." It is scenery. 

A Schoolhouse is north of Plushieland. "A small schoolhouse atop a grassy hill, red walls delineated with thick, playful strokes. The roof is cozily off-kilter a bell perched on top. A crooked wooden sign near the door reads 'Neoschool' in swirling letters. Curled on the front steps are a couple of petpets. To the north is a meadow. To the east is a Library." 

The Library is east of the Schoolhouse. "Tall bookshelves line the walls, filled with texts on Neopian lore. A sturdy oak desk sits in the center, where the librarian watches over her collection."

The Librarian is a woman in the Library. "The librarian is a wise-looking Aisha with spectacles perched on her nose. She greets you with a knowing smile." The Librarian carries Illusen's Spell Book.

The Floral Encyclopedia is in the Library. "Beside her is a book titled [italic type]Floral Encyclopedia[roman type]. Perhaps it holds a clue."

Instead of asking the Librarian about something:
	say "The librarian adjusts her spectacles and gives you a knowing smile. 'If you wish to take out a book, you must first provide an offering of knowledge. Bring me something that demonstrates your wisdom, and then we shall talk.'"


Instead of reading the Floral Encyclopedia:
	say "The book describes three flowers:[line break] 
	- The Starpetal Bloom, which glows faintly in the moonlight.[line break] 
	- The Mindroot Blossom, said to enhance memory and wisdom.[line break] 
	- The Dewshade Lily, known for its calming properties.[line break]"

The Meadow is north of the Schoolhouse. "A lush, open field filled with vibrant flowers. The air is thick with the scent of wild blooms."

A flower is a kind of thing. A Starpetal Bloom is a flower in the Meadow. A Mindroot Blossom is a flower in the Meadow. A Dewshade Lily is a flower in the Meadow.

Instead of taking a flower:
	say "You carefully pluck [the noun], hoping it is the one the librarian seeks.";
	now the player carries the noun.

Instead of giving a flower to the Librarian:
	if the noun is the Mindroot Blossom:
		say "The librarian inspects the flower and nods. 'Well done. You have proven your wisdom. Here is the book you seek.' She hands you Illusen's Spell Book.";
		now the player carries Illusen's Spell Book;
	otherwise:
		say "The librarian shakes her head. 'You must first bring me the Flower of Knowledge from the meadow. But only the right one.'"
 
[A stuffed animal is a kind of thing.]

The jubjub is a person in Plushieland. Understand "jubjub" as the jubjub.
The description of the jubjub is "A bright red jubjub. It's pacing around in tight circles, vibrating with nervous energy."

Understand "talk to [someone]" as asking it about.

Instead of asking the jubjub about "illusen":
	say "'You haven't seen Illusen's spell book, have you?' asks the jubjub. 'It should be nestled somewhere in the Schoolhouse Library...'";
	
Instead of asking the jubjub about something:
	say "'Oh, hello,' whispers the jubjub. 'I'd like to stay and chat, but I'm in the middle of one of Illusen's quests.'"

[Instead of asking jubjub about "illusen":
	say "'You haven't seen Illusen's spell book, have you?' asks the jubjub. 'It should be nestled somewhere in the Schoolhouse...'".]


The hedgehog is a person in Plushieland. Understand "hedgehog" as the hedgehog.
The description of the hedgehog is "The hedgehog is covered in tiny, harmless quills, giving it a spiky yet soft appearance."
Instead of asking the hedgehog about something:
    say "'I've been rolling around here forever!' the hedgehog giggles. 'Want to roll with me?'"

The djungelskog is a person in Plushieland. Understand "djungelskog" as the djungelskog.
The description of the djungelskog is "A large, soft bear with a slightly slouched posture, as if it's ready for a cozy nap."
Instead of asking the djungelskog about something:
    say "'Hello,' the djungelskog says in a slow, deep voice. 'Come sit, rest. There's room for everyone on my belly.'"


Table of Conversation
Topic	Response
"crayon"	"I love crayons"
"narwhal"	"I love narwhals"





[INDEX]

Section 5 - INDEX

A journal is a thing. It is portable. The description of the journal is "[fixed letter spacing]

.      .--.                   [line break]
.  .---|__|           .-.     [line break]
.--|===|--|_          |_|     [line break]
|  |===|  |'\     .---!~|  .--[line break]
|%%|   |  |.'\    |===| |--|%%|[line break]
|%%|   |  |\.'\   |   | |__|  |[line break]
|  |   |  | \  \  |===| |==|  |[line break]
|  |   |__|  \.'\ |   |_|__|  |[line break]
|  |===|--|   \.'\|===|~|--|%%|[line break]
^--^---'--^    `-'`---^-^--^--[variable letter spacing]

A worn out journal in which you've taken to scrawling your dreams.

TABLE OF CONTENTS:[line break][line break]
	0. BEDROOM [line break]
	1.  TOYBOX[line break]
	2.  [if the player is carrying the narwhal]PLUSHIELAND[else]?????????[end if][line break]
	3. ?????????
	
	[italic type]You can read the journal by typing: read page N of journal[roman type].".
	

	
The journal is an openable closed container. A journal has a number called the last page read.

Check reading journal:
	If the player is not carrying the journal, say "You're not reading anything." instead;

Carry out reading it in:
	read page number understood.

Carry out reading:
	say "You need to specify a page to read.";

Table of Book Contents
page	content
0	"BEDROOM"
1	"TOYBOX"
2	"[if the player is carrying the narwhal]PLUSHIELAND[else]?????"
3	"??????"
4	"??????"

To read page (N - a number):
	if N is 0:
		say "A thunderous crack cuts through the air. Everything begins to spin. You watch as everything around you dissolves and reforms until your eyes begin to burn from the distortion.";
		move player to Bedroom;
		reject the player's command;
	if N is 1:
		say "A thunderous crack cuts through the air. Everything begins to spin. You watch as everything around you dissolves and reforms until your eyes begin to burn from the distortion.";
		move player to Toybox;
		reject the player's command;
	if N is 2:	
		if the player is carrying the narwhal:
			say "A thunderous crack cuts through the air. Everything begins to spin. You watch as everything around you dissolves and reforms until your eyes begin to burn from the distortion.";
			move Emma to Plushieland;
			move player to Plushieland;
			reject the player's command;
		otherwise: 
			say "Page [N] appears to be blank.";
	else:
		say "Page [N] appears to be blank.".

Understand the command "read" as something new.
Understand the command "write" as something new.

Understand "read [something]" or "consult [something]" or "read in/from [something]" as reading. Reading is an action applying to one thing.

Understand "write [something]" or "write in [something]" or "write dream in [something]" as writing. Writing is an action applying to one thing.

Understand "read [number] in/from/of [something]" or "read page [number] in/from/of [something]" or "look up page [number] in/from/of [something]" or "consult page [number] in/from/of [something]" as reading it in. Reading it in is an action applying to one number and one thing.

[RECORD PLAYER]

Understand "play [something]" as playing. Playing is an action applying to one thing.

[After drawing:
	say "(with the crayons) You doodle a bunny on the computer.".

After drawing doodle newsprint:
	say "(with the crayons) You doodle a unicorn on the newsprint.".

After drawing doodle on book:
	now bookDrawn is 1;
	say "(with the crayons) You doodle a bunny on the book.".
]
After writing journal:
	now bookWrite is 1;
	say "(with the pen) Your hands move of their own accord.".

After playing record player:
	now the command prompt is "> Select a song: ".
	
After taking the narwhal:
	say "It's a ragged thing. An old hurt vibrates through you. You can't place why. From faraway, the journal vibrates.";

After reading a command when the command prompt is "> Select a song: ":
	if the player's command matches "1":
		say "[italic type]Phoebe Bridgers fills the room.";
	else if the player's command matches "2":
		say "[italic type]Fall Out Boy fills the room.";
	else if the player's command matches "3":
		say "[italic type]Pussy Riot fills the room.";
	else if the player's command matches "4":
		say "[italic type]Twice fills the room.";
	else if the player's command matches "5":
		say "[italic type]Twenty One Pilots fills the room.";
	now the command prompt is ">".

After opening suitcase:
	say "Inside is a yearbook, a record player, a binder, and a 3D printed narwhal, amongst other odds and ends.[line break]The closet smells dusty.".

Every turn when record player is switched on:
	if the location of the player is the location of the record player:
		if songX is 0:
			say "[italic type]Phoebe Bridgers fills the room.";
		else if songX is 1:
			say "[italic type]Fall Out Boy fills the room.";
		else if songX is 2:
			say "[italic type]Pussy Riot fills the room.";
		else if songX is 3:
			say "[italic type]BETWEEN FRIENDS fills the room.";
			now songX is 1;
		else if songX is 4:
			say "[italic type]Porter Robinson fills the room. You're on the verge of tears, but you're not sure why.";
		else if songX is 5:
			say "[italic type]Twenty One Pilots fills the room.";
		now songX is a random number from 1 to 5.

Release along with an interpreter.
