"Dreamland" by Melissa Liu

roomCount is a number that varies. roomCount is 0.
songX is a number that varies. songX is 0.
bookDrawn is a number that varies. bookDrawn is 0.
bookWrite is a number that varies. bookWrite is 0.
emmaName is a number that varies. emmaName is 0.
drawingHint is a number that varies. drawingHint is 0.

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


dream: early 2000s
high school 
computer dream

endings:
	she coded it all up as fantasy escapism
	god complex ?
	imaginary friend
	she erased her own memory
	
	she did something bad (memory) - first dream
	
]

When play begins:
say "You wake up to the ringing of your alarm. Images of a strange dream drift through your mind, but when you try to conjure them, you draw a blank.

(´｡• ᵕ •｡`) ♡
".

[After printing the banner text:
    say "[line break][italic type]What we share may be a lot like a traffic accident but we get one another. We are survivors of each other. We have been shark to one another, but also lifeboat. That counts for something. [line break]".]

Teleporting is an action applying to one visible thing. Understand "go to [any room]" as teleporting. Understand "go [any room]" as teleporting. Carry out teleporting: move the player to the noun.

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

The Bedroom is a room. "
.   \_/       .:'    .:'    .:'[line break]
-=(_)=- /\ ||    /\||    /\||[line break]
.   / \     //\\|   //\\|   //\\|[line break]
.          //  \\  //  \\  //  \\[line break]
.         //    \ ^/    \^/    \\[line break]
.         |  ( )  ||  ( ) || ( ) |[line break]
.         |       ||      ||      |%[line break]
  &%&--==--&%-==--%&[line break]

[if roomCount is 0]Your bedroom is unadorned. The walls are chalky and the ground bare. The only decoration is a poster of The Killers. A lightbulb hangs from the ceiling. A tiny window is opened a crack, and the sound of faint static comes through.[else if roomCount is 1]You step into the bedroom and are flooded with emotion. Every inch of its surface now looks as if it's been dipped in. The walls are pulsating and warm, shifting from blue to red to violet. [end if] In the distance your kitchen fridge hums.

[if roomCount is 0]In the corner your monitor blinks wearily. You must've forgotten to turn it off. [else if roomCount is 1]Lo-fi indie is playing from your computer. You notice movement on the screen.[end if] Beside you is a worn out drawer. To the left is a cramped closet.
".

A poster is here. It is fixed in place. A lightbulb is here. It is fixed in place. It is a device. It is switched off. A drawer is here. It is a container. It is openable. It is closed. It is locked. It is fixed in place. A journal, crayons, and glasses is inside the drawer. The glasses is wearable.
The description of drawer is "It's covered with a layer of dust and lined with faint scratches. You can make out a tiny keyhole in the upper right corner.".
The description of lightbulb is "A single lightbulb. A string dangles.".
The description of poster is "A relic of your emo era. The corners have frayed over the years. ".
The description of crayons is "A bright yellow box. On it are instructions on how to draw certain shapes: a bunny, a lion, a narwhal, and a unicorn."
The description of glasses is "A wacky looking pair of with black frames and blue-purple swirling lenses. On the side are the words [italic type] Sentence Scrambler."

After opening the drawer:
	say "You pull open the drawer. A journal, a box of crayons, and a pair of glasses	 lies inside. The journal beckons to you.".

After putting on glasses:
	say "You put on the pair of glasses and the world turns all sorts of odd colours. Guess you need to change your prescription.".

A desk is here. It is a supporter. It is fixed in place. A ballpoint pen is on the desk. It is fixed in place. A computer is on the desk. A computer is a device. It is switched off. It is fixed in place. The description is "[if noun is switched off]A yellowing Windows XP that's seen better days.[else if roomCount is 0]The computer is open to a game development software. You feel unsettled.[else if roomCount is 1]Your screen is being populated with a barrage of popups. The same message is written on them. 'Your dreams await. \( ^^)/'.[else if roomCount is 2]
A browser is open to a new tab. In the bookmarks are several pages - neopets, AOL, club penguin. Tne neopets browser has a username and password field. [end if] "

Understand "monitor" or "laptop" as computer.
Understand "lamp" or "light" as lightbulb.
Understand "Killers" or "wall" as poster.

Understand "click [text]" or "click on [text]" as clicking. Clicking is an action applying to one topic.

Carry out clicking:
	if the player's command matches "click neopets":
		say "
		
		
		Enter Username: ".



Rule for printing the locale description of Bedroom: stop.

Carry out switching on the lightbulb:
	say "You pull on the lightbulb. Things seem a little brighter.".

Carry out switching off lightbulb:
	say "The room dims.".

Carry out switching on computer:
	say "The monitor lights up.".

Carry out switching off computer:
	say "The monitor shuts off. The fan goes silent.".

A newsprint is here. "A crumpled newsprint lies wrinkled on the floor." It is fixed in place. The description is "You straighten out the sheet of newspaper, handling it delicately. [if roomCount is 0]You can't quite make out the words. Your head is spinning.[else if roomCount is 1] The page contains two girls playing in a sandbox. The smaller of the two is labouring over a crudely formed sand sculpture. The other is carelessly sprawled across the sand, her body covered in dust. You study the small stature, the neat braids, the wrinkled clothing. They look around preschool age. [paragraph break]¯¯̿̿¯̿̿'̿̿̿̿̿̿̿'̿̿'̿̿̿̿̿'̿̿̿)͇̿̿)̿̿̿̿ '̿̿̿̿̿̿\̵͇̿̿\=(•̪̀●́)=o/̵͇̿̿/. You can't make out the caption.[end if]".

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

The Closet is a room. "It's empty except for a single cardbord box. You feel cramped." A cardboard box is here. A cardboard box is openable and closed. The cardboard box is a container. A yearbook, record player, and narwhal is in the cardboard box.

A yearbook is a container. It is openable. The description is "[italic type][if roomCount is 0] ??? ???? ???? ???? You can't make out the text right now.[else if the player is wearing the glasses]Class of XXYZ[end if][roman type].".
A narwhal is a thing. The description is "An old hurt vibrates through you. You can't place why.".
A record player is a device. It is switched off. It is fixed in place. The description is "A vintage turntable. It's loaded with a vinyl - Best of 2010s Hits.".

Section 2 - KITCHEN

The Kitchen is west of the Bedroom. "Your kitchen has seen better days. A fridge lies in one corner. Near the door is a row of plants with yellowing leaves. At the north corner is a door that leads onto the street."

The silver key is a thing. The description is “A stainless steel key, slightly bigger than your thumb. It's wet with your saliva.” The matching key of the drawer is the silver key. The silver key is nowhere.

A jug is here. 

The plant is a thing. It is here. It is fixed in place. 
Understand "water [something]" as watering. Watering is an action applying to one thing.

After watering the plant:
	if the player is carrying the jug:
		say "You water the plant with the jug. The plant perks up a little.";

A note is here. It is fixed in place. A fridge is here. The fridge is fixed in place.  The description of the fridge is "You're bizarrely attached to your fridge. It's charming in its age, rickedy, humming in a contented manner. It's covered with  and a note is pinned to it." It is a container. It is openable. It is closed. The description of the note is "There's a note stuck to the fridge. It reads [if roomCount is 0] ??? ??? ????????? ???? You can't make out the text [else if roomCount is 1] 'Where did you go? Come play with me in Dreamland /( ^^)/[end if].'".

After opening the fridge, say "The fridge contains [if roomCount is 1]a vial filled with black fluid, saltine crackers, a slice of strawberry shortcake, a potion labelled 'Drink Me' [else if roomCount is 0]??? ??? ????????? ???? You can't make out the text.[end if][no line break] and curiously, a piece of candy. The candy beckons to you."

The candy is edible. The description is "A lychee sweet. It glows mysteriously."  The fridge contains a black vial, a 'Drink Me' potion, shortcake, crackers and candy.  The black vial, 'Drink Me' potion, shortcake, crackers are not edible. The black vial, 'Drink Me' potion, shortcake, crackers are fixed in place.

After taking the candy:
	say "You pocket the candy. It feels heavy, like nostalgia."

After eating the candy:
	say "You unwrap the candy and pop it in your mouth. It tastes of a strange medley of flavours - vanilla custard, citrus, a hint of buttered toast. Delightful.

A sudden prickling rips through your throat, like you've swallowed something you shouldn't have. You double over, coughing, clutching the counter for balance. There's something lodged in your throat.

[italic type]Clang![roman type] Out comes a small piece of metal. You pick it up, bewildered. It's a small key.";
	now the player is carrying the silver key;
	now roomCount is 1;


Section 3 - Toybox

Toybox is a room. "
|                  /^\   / -- )[line break]
|                 / | \ (____/[line break]
|                / | | \ / /[line break]
|               /_|_|_|_/ /[line break]
|                |     / /[line break]
| __    __    __ |    / /__    __    __[line break]
|  |__|  |__|  |.   / /|  |__|  |__|  |[line break]
|__            ____/ /___           __|[line break]
   |          / .------  )         |[line break]
   |         / /        /          |  TOYBOX[line break]
   |        / /        /           |[line break]
~~~~~~~~~~~~-----------~~~~~~~~


You open your eyes to pure whiteness. Every bit of empty space is blue lines, pink margins...

Margins! That's it. The world you're in is rendered on notebook paper. Objects are flat, paperlike, outlined in waxy lines. You look down at your hands and suppress a cry of shock. You're wearing a triangular smock of purple and your body is composed of black lines. You unfurl your fingers and watch as the lines retract and relax.

You take in the rest of your surroundings. You seem to be in a playground of sorts. There's a set of swings and a playstructure in a garish primary colours. In the distance is a forest, the paper trees swaying in the wind.

A girl no older than five is playing in the field. She's crawling through the grass, plucking out a stem here and there." Toybox is a room with printed name "Toybox (Dreamland)".

[A doodlebox machine is in the Toybox. It is fixed in place and opaque. ]

A bunny is here. It is fixed in place. A clover is here. It is fixed in place. A dandelion is here. It is fixed in place.

Understand "draw [something]" or "draw a [something]" as drawing. Drawing is an action applying to one thing.

Understand "draw [text]" as drawrandoming. Drawrandoming is an action applying to one topic.

Check drawing:
	If the player is not carrying the crayons, say "You don't have anything to draw with." instead.

Report drawing:
	now the noun is in Toybox;
	now the player is carrying the noun;
	say "(with the crayons) You doodle a [noun]. Looks cute. The air sizzles, and with a pop, the [noun] is splits from its surroundings. Something drops in your pocket.".

Check drawrandoming:
	If the player is not carrying the crayons:
		 say "You don't have anything to draw with" instead.

Carry out drawrandoming:
	say "(with the crayons) You doodle a [the topic understood]. Looks cute.".

Rule for printing the locale description of Toybox: stop.

Section 3,1 - EMMA

emmaTalk is a number that varies. emmaTalk is 1.

Emma is here. Emma is a person. The description of Emma is "A small girl wearing a checkered dress. Her hair is in neat plaits and her socks are mismatched. She looks familiar." Understand "girl" or "child" or "kid" as Emma.

Instead of telling someone about something, try asking the noun about it. Instead of answering the noun that something, try asking the noun about it.

Understand "ask [someone] [text]" as asking it about.
Understand "tell [someone] [text]" as answering it that. Understand "tell [someone] that [text]" as answering it that.
Giving it about is an action applying to two things. Understand "give [someone] [something]" or "give [something] to [someone]" as giving it about.

Carry out giving something about someone:
	say "Emma cries out in joy. A [the second noun]!";
Instead of giving narwhal to Emma:
	say "She scoffs at the plushies. 'I'm not talking about a narwhal, silly! Mr. Narkins is way smarter at that thing. We used to play Faerie Bubbles together.";

Instead of giving dandelion to Emma:
	say "Emma looks up at you in wonder. 'A dandelion', she whispers reverentially. 'I've never seen one before.'

	You tell her they're commonplace where you're from. 'That can't be right,' she says. 'My grandma says they went extinct long ago. They were killed, you know. For being too resilient. She cups the fluffy white thing delicately. 'C'mon! Let's wish together.'

	You put your heads together and your breaths create a flurry of white as the spores release into the air. You stare at this strange girl, wide-eyed with awe, twirling the empty stem in her hands. You're drawn to her uncomplicated joy.

	'I wished for a friend,' she says abruptly. She gazes at you in wonder. 'I've never had a friend. Will you be my friend?";
	now the command prompt is "> Befriend Emma? Y/N";

The crayola key is nowhere. It is fixed in place.

After reading a command when the command prompt is "> Befriend Emma? Y/N":
	if the player's command matches "Y":
		say "Her face blooms with delight. 'YAAAAY', she cries. 'We can all be friends now. Me, you, Mr. Narkins.

		Her face darkens for a second. 'Have you seen him, by the way? He's a sea unicorn.' You feel suction around you, like you're going through a vacuum. Everything blurs.";
		now the player is carrying the crayola key;
		move player to Bedroom;
	else if the player's command matches "N":
		say "Emma deflates for just a second. Then she shrugs it off, and says, 'That's okay.'";
	else:
		say "[italic type]Twenty One Pilots fills the room.";
	now the command prompt is ">".


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
Instead of asking Emma about "dream/dreaming":
    say "[if emmaName is 1]Emma[else]The girl[end if]is crestfallen. So it's true. You think we're only dreaming.".
Instead of asking Emma about "hi":
    say "[if emmaName is 1]Emma[else]She[end if] crushes you in a hug. You found me!".
Instead of asking Emma about "drawing/draw/art/doodle/":
    say "C'mon! Let's draw together.".
Instead of asking Emma about "crayons":
    say "Waahhhh! Crayons! Can I eat one?.".
Instead of asking Emma about "wish/magic":
    say "You feel it too, don't you? whispers [if emmaName is 1]Emma[else]the girl[end if]. This is a magic place.".
Instead of asking Emma about "looking/find/doing/grass/stem/meadow":
    say "[if emmaName is 1]Emma[else]The girl[end if] looks solemn, 'I'm looking for something. It's a [italic type]special[roman type] plant. My grandma told me it's got special powers, and I need it to make a wish.".
Instead of asking Emma about "wish":
    say "Huuuh? I can't tell you! 'Cause if I did, it wouldn't come true!".
Instead of asking Emma about "dandelions/flowers/dandelion":
    say "[if emmaName is 1]Emma[else]The girl[end if] looks up at you in wonder. So you do know the story! Could'ya help me look? Pretty please!".
Instead of asking Emma about "parents":
    say "[if emmaName is 1]Emma[else]The girl[end if] goes quiet for a long time. When she speaks, her voice is shaking.".
Instead of asking Emma about "fuck/shit/bitch/ass/cunt/penis/boobs":
    say "[if emmaName is 1]Emma[else]The girl[end if] covers her ears and turns red. 'Stop it with the bad words or I'll tell on you!'".
Instead of asking Emma about something:
	if emmaTalk is 1:
		say "Emma doesn't seem to hear you. She seems lost in a daydream.";
	else if emmaTalk is 2:
		say "Emma is too busy digging through the meadow.";
	else if emmaTalk is 3:
		say "Emma is humming to herself.";
	now emmaTalk is a random number from 1 to 3.
Instead of asking Emma about "world/where/toybox/notebook/paper", say "She looks at you quizzickly. 'Whatdya mean? You don't remember?'".

Section 4 - Pillowfort




Plushieland is north of Toybox. "You wake up in world of cotton. Everything you see is rendered in a shade of pastel - the turgid river, the sky, the meadows in front of you. A girl around the age of seven is digging up flowers."

Table of Conversation
Topic	Response
"crayon"	"I love crayons"
"narwhal"	"I love narwhals"





[INDEX]

Section 5 - INDEX

A journal is a thing. It is portable. The description of the journal is "A worn out journal in which you've taken to scrawling your dreams." The journal is an openable closed container. A journal has a number called the last page read.

Check reading journal:
	If the player is not carrying the journal, say "You're not reading anything." instead;

Carry out reading it in:
	read page number understood.

Carry out reading:
	let N be a random number between 1 and the 4; now the number understood is N;
	say "You flip the pages randomly and arrive at page [the number understood]:[paragraph break]";
	try reading the number understood in the journal.

After unlocking journal with key:
	 now the description of the journal is "
	TABLE OF CONTENTS[line break]
	1  TOYBOX[line break]
	2  ??????????[line break]
	3  ??????????[line break]".


Table of Book Contents
page	content
1	"TOYBOX"
2	"???????"
3	"??????"
4	"???????"

To read page (N - a number):
	if there is a content corresponding to a page of N in the Table of Book Contents:
		choose row with a page of N in the Table of Book Contents;
		say "A thunderous crack cuts through the air. Everything begins to spin. You watch as everything around you dissolves and reforms until your eyes begin to burn from the distortion. [N]. '[content entry]'[paragraph break]";
		move player to Toybox;
	otherwise:
		say "Page [N] appears to be blank."

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

After opening cardboard box:
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
			say "[italic type]Twice fills the room.";
			now songX is 1;
		else if songX is 4:
			say "[italic type]Bruno Mars fills the room. You're on the verge of tears, but you're not sure why.";
		else if songX is 5:
			say "[italic type]Twenty One Pilots fills the room.";
		now songX is a random number from 1 to 5.
