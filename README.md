# CSC151Project
Design Statement

While incorporating color variation, our project mainly explores shapes in our project with certain level of predictability.
In the following design statement, we will first introduce the calling and product of the procedure as well as its purpose. 
Second, we will delve deeper into the elements and principles of two dimensional design used in our procedure. Then we will
evaluate the colors used in forming the products of the procedure and give a summary of the implications of our procedure.
When the user calls our procedure, he/she will input three parameters, a number between 1~999, the width and the height of
the image generated. Our main idea is to develop the procedure into a game which users can play it by making successive calls
with different first parameter. The game has five levels and each correspond to 10 of the possible 1000 pictures. First the
user start with the starting picture and get the clue(by counting certain geometric drawings -stars, squares that correspond 
to the three digit of next picture's index) from the picture generated to guess the index(the first parameter) of the next
picture he/she is supposed to call. By calling right pictures in turn the user can pass five levels(that is 50 key pictures)
and reach the final picture. However, if the user make the wrong call, the picture generated will be clearly different than 
expected thus can inform the user that he/she has made the wrong call.

In our project we intend to use several design elements such as Picture plane, Positive space/shape, Negative space/shape, Line,
Value, and Color. The fifty pictures representing five levels will be in the form of magic circles with five kinds of patterns
and the rest pictures will be in the form of a magic tree. (more specific please see technique statement).

Also we will follow various design principles such as Balance, Unity and Variety, Scale, Proportion, and Rhythm. Our pictures will
be in certain symmetry with imbalance in some point(Balance). The pattern of the picture generated will include grouping, repetition,
proximity, continuity, pattern and grid(Unity and Variety). Also part of the picture will contain concentric geometric drawings(Scale).
Meanwhile, users are expected to see different patterns(lines, squares, circles) with different sizes(Proportion) and multiple units 
in a deliberate pattern(Rhythm) present in one picture. (To understand more specifically, please see the attached sketches of images)

After studying the color theory, we plan to give each ten of the 50 key pictures a theme color while using the same theme color
in the rest of the pictures(all of the pictures will use dark colors, that is a colors with small Value, as base colors). The
corresponding theme colors of each of five levels will be green, red, yellow, blue and white(mostly Primary colors and Secondary
colors) with the other pictures use pink-purple as a theme color. Each of the theme color is intend to be considerably bright, 
that is with large Saturation. Because in our pictures we will use primarily lines and hollow graphics than color blocks, the
user can interpret the pictures as drawing with pens on a paper.

In conclusion, our project is intend to engage the people who call our procedure in the game while showing them magical pictures.
For more technical details and illustrations please read the technique statement and see the sketched images.

Reference:
www.cs.grinnell.edu/~klingeti/courses/f2016/csc151/assignments/projectdesign statement technique statement
http://www.cs.grinnell.edu/~klingeti/courses/f2016/csc151/readings/design-elements-reading.html
http://www.cs.grinnell.edu/~klingeti/courses/f2016/csc151/readings/design-and-color-reading.html
http://www.bartleby.com/141/strunk5.html

technique statement

We split the number the user put in into three parts: the hundreds digit, the tens digit, and the units digit, and each of them
will control a certain character of our image.

We mainly use GIMP tools(selection, stroke, draw line...), turtles and image-compute for both kind of pictures (the magic circle
and magic tree).

In the three sample we provide for the magic tree, the non-key pictures, the hundreds digit controls the color(numbers 0-9 correspond
to 10 different colors, 0 means nothing at hundreds digit), the tens digit controls the distribution of circles(umbers 0-9 correspond
to 10 different patterns), and the units digit controls the quantity of concentric circles(0 means 1 circle,5 means 6 circles etc.).
This will ensure us to generate 950 different magical trees.(A condition is used here to ensure if the right number for the game is
inputed, a magic circle will be generated instead of a magic tree).

On the other hand, we will also use GIMP tools, turtles and image-compute for the 50 key images.

The mathematical techniques we used include conic section, trigonometric function, etc. We used conic section computation to draw the
oval on the background, and trigonometric function to draw the halo.


