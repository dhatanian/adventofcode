package day13

object Data {
  val initialState = """                                                                                                             /----------------------------\
                       |            /-------------------------------------------------------------\                                  |                            |
                       |            |                               /-------------------\         |                    /-------------+---------------\            |
                       |            |                               |     /-------------+---------+--------------------+------\      |              /+------------+---------\
                       |            |                               |     |         /---+---------+--------------------+------+------+--------------++---\        |         |
                       |/-----------+-\                             |     |         |   |   /-----+--------------------+------+----\ |              ||   |        |         |
                       ||           | |                   /---------+-----+---------+---+---+-----+--------------------+------+----+-+------\       ||   |        |         |
                       ||        /--+-+-------------------+---------+-----+---------+---+---+-----+------\             |      |    | |      |       ||   |        |         |
                       ||        |  | |                   |         |     |         |   |   |     |      |             |   /--+----+-+---\  |       ||   |        |         |
                       ||        |/-+-+-------------------+-------\ |     |     /---+---+---+-----+------+-------------+---+--+----+-+---+--+-------++---+--------+---\     |
                       ||        || | |                   |       | | /---+-----+---+---+---+\    |      |  /----------+---+--+----+-+---+--+------\||   |        |   |     |
                       ||        || | |                   |       | | |   |     |   |   |   ||/---+------+--+----------+---+\ |    | |   |  |      |||   |        |   |     |
                       ||    /---++-+-+------->------\    |       | | |   |     |   |   |   |||   |      |  |   /------+---++-+----+-+---+--+-\    |||   |        |   |     |
                       ||    |   || | |              |    |       | | |   |     |   |   |   |||   |      |  |   |      |   || |    | |   |  | |    |||   |        |   |     |
                       |\----+---++-+-/              |    |       |/+-+---+-----+---+---+---+++---+------+--+---+------+---++-+----+-+---+--+-+----+++---+--------+\  |     |
                       |  /--+---++-+--------------\ |/---+-------+++-+---+-----+---+---+---+++--\|      |  |   |      |   || |    | \---+--+-+----+++---+--------/|  |     |
                       |  |  |   || |              | ||/--+-------+++-+---+-----+---+---+---+++--++------+--+---+------+--\|| |    |     |  | |    |||   |         |  |     |
                       |  |  |   || |/------->----\| |||  |       ||| |  /+-----+---+---+---+++--++------+--+---+------+--+++-+----+-----+--+-+----+++---+-----\   |  |     |
                       |  |  |   || ||            || |||  |       ||| |  ||     |   |  /+---+++--++------+--+-\ |      |  ||| |    |     |  | |    |||   |     |   |  |     |
                       |  |  |   || ||            || |||  |       ||| |  ||     |   |  ||   |||  ||      |  | | |      | /+++-+----+-----+--+-+----+++---+-\   |   |  |     |
                       |  |  |   || ||            || |||/-+-------+++-+--++-----+---+--++---+++--++------+--+-+-+------+-++++-+----+-----+--+-+----+++---+-+\  |   |  |     |
                       |  |  |   || ||            || |||| |       ||| |  ||     \---+--++---+++--++------+--+-+-+------+-++++-+----+-----+--+-+----+++---+-++--+---+--/     |
                       |  |  |   || ||            || |||| |       ||| |  ||         |/-++---+++--++------+--+-+-+------+-++++-+----+-----+--+-+----+++-\ | ||  |   |        |
                       |  |  |   || ||            ||/++++-+-------+++-+--++---------++-++---+++--++------+--+-+-+------+-++++-+----+\/---+--+-+----+++-+-+\||  |   |        |
                       |  |  |   || ||            ||||||| |       |\+-+--++---------++-++---+++--++------+--+-+-+------+-++++-+----+++---+--+-+----+++-+-++++--+---/        |
                       |  |  |   || ||            ||||||| |       | | |  ||    /----++-++---+++--++------+--+-+-+------+-++++-+----+++---+--+-+----+++-+\||||  |            |
                       |  |  |   || ||            |||||||/+-------+-+-+--++----+----++-++---+++--++------+--+-+-+------+-++++-+----+++---+--+\|    ||| ||||||  |            |
                       |  |  |   || ||            |||||||||       | | |  ||    |    || ||/--+++--++------+--+-+-+----\ | |||| |    |||   |  |||    ||| ||||||  |            |
                       |  |  |   || ||       /----+++++++++\      | | |  ||    |  /-++-+++--+++--++------+--+-+-+\   | | |||| |    |||   |  |||    ||| ||||||  |            |
                       |/-+--+---++-++-------+----++++++++++--\   | | |  ||  /-+--+-++-+++--+++--++------+--+-+-++---+-+-++++-+----+++---+--+++----+++-++++++--+----\       |
                       || |  |   \+-++-------+----++++++++++--+---+-+-+--++--+-+--+-++-+++--+++--++------/  | | ||   | |/++++-+---\|||   |  |||    ||| ||||||  |    |       |
                       || |  |    | ||       |    ||||||||||  |   | | |  ||  | |  | || |||  v||  ||         | | ||   |/++++++-+---++++---+--+++----+++-++++++--+--\ |       |
                       || |  |    | ||       |    ||||||||||  |   | | |  ||  | |  | || |||  |||  ||         | | ||   |||||||| |   ||||   |  |||    ||| ||||||  |  | |       |
                       || |  |  /-+-++-------+----++++++++++--+---+-+-+--++--+-+--+-++-+++--+++--++-------\ | | ||   |||||||| |   ||||   |  |||    ||| ||||||  |  | |       |
                       || |  |  | | ||       |    ||||||||||  |   | |/+--++--+-+--+-++-+++--+++--++--\    | | | ||   |||||||| |   ||||   |  |||    ||| ||||||  |  | |       |
                       || |  |  | | ||       |    ||||^|||||  |   | ||\--++--+-+--+-++-+++--+/|  ||  |    | | | ||   |||||||| |   ||||   |  |||   /+++-++++++--+--+-+\      |
                       || |  |  | | ||       |/---++++++++++--+---+\\+---++--+-+--+-++-+/|  | |  ||  |    | | | \+---++++++++-+---++++---+--++/   |||| ||||||  |  | ||      |
                       || |  |  | | ||       ||   ||||||||||  |  /++-+---++--+-+--+-++-+-+--+-+--++--+----+\| |  |   |||||||| |   ||||   |  ||    |||| ||||||  |  | |v      |
                       || |  |  | | ||      /++---++++++++++--+--+++-+\  ||/-+-+--+-++-+-+--+-+--++--+--\ ||| |  |   |||||||| |   ||||   |  ||    |||| ||||||  |  | ||      |
                       ||/+--+--+-+-++------+++---++++++++++--+--+++\||  ||| | |  | \+-+-+--+-+--++--+--+-+++-+--+---++++++++-+---++++---+--++----++++-++/|||  |  | ||      |
                       |\++--+--+-+-++------+++---++++++++++--/ /++++++--+++-+-+--+--+-+-+--+-+--++--+--+-+++-+--+---++++++++-+---++++\  |  ||    |||| || |||  | /+-++------+\
                       | ||  |  | | ||      |||   ||||||||||    |||||||  ||| | |  |  | | |/-+-+--++--+--+-+++-+--+---++++++++-+---+++++--+--++----++++-++-+++\ | || ||      ||
                       | ||/-+--+-+-++------+++---++++++++++----+++++++--+++-+-+--+--+-+-++-+-+--++--+--+-+++\|  |   |||||||| |   |||\+--+--++----++++-++-/||| | || ||      ||
                       | ||| |  | | ||      |||   ||||||||||/---+++++++--+++\| |  |  | \-++-+-+--++--+--+-++++/  |   |||||||| |   ||| |  |  ||    |||| ||  ||| | || ||      ||
                       | ||| |  | | ||      |||   |||||||||||   |||||||  ||||| |  |  |   || | |  ||  |  | ||||   |   |||||||| |/--+++-+--+--++----++++-++-\||| | || ||      ||
                       |/+++-+--+-+-++------+++---+++++++++++---+++++++--+++++-+--+--+---++-+-+--++--+--+-++++---+\  |||||||| ||  ||| |  |  ||    |||| || |||| | || ||      ||
                       |||||/+--+-+-++------+++---+++++++++++---+++++++-\||||| |  |  |   || | |  ||  |  | ||||   ||/-++++++++-++--+++-+--+-\||    |||| || |||| | || ||      ||
                       |||||||  | | ||      \++---+++++++++++---++++++/ |||||| |  | /+---++-+-+--++--+--+-++++---+++-++++++++-++--+++-+--+-+++----++++-++-++++-+-++-++---\  ||
                       |||||||  | | ||       ||   |||||||||||   ||||||  ||\+++-+--+-++---++-+-+--++--+--+-++++---+++-++++++++-/|  ||| |  | |||    |||| || |||| | || ||   |  ||
                       |||||||  | | || /-----++---+++++++++++---++++++--++-+++-+--+-++---++-+-+--++\ |  | ||||   ||| ||||||||  |  ||| ^  | |||    |||| || |||| | || ||   |  ||
                       |||||||  | | || |     ||/--+++++++++++---++++++--++-+++-+--+-++---++-+-+--+++-+--+-++++---+++-++++++++\ |  ||| |  | |||    |||| || |||| | || ||   |  ||
                       |||||||  | | || |     |||  |||||||||||   ||||||  || ||| |  | ||/--++-+-+--+++-+--+-++++--\||| |\+++++++-+--+++-+--+-+++----++++-++-++++-+-+/ ||   |  ||
                       |||||||  | | || |     |||  |||||||||||   ||||||  || ||| |/-+-+++--++-+-+--+++-+--+-++++--++++\| ||||||| |  ||| |  | |||    |||| || |||| | |  ||   |  ||
                       |||||||  | | || |     |||  |||||||||||   ||||||  || ||| || | ^||  || | \--+++-+--+-++++--++++++-+++++/| |  ||| |  | |||    |||| || |||| | |  ||   |  ||
                       ||||||\--+-+-++-+-----+++--+++/|||||||   ||||||  || ||| || | |||  || |    ||| |  | ||||  |||||| ||||| | |  ||| |  | |||    |||| || |||| | |  ||   |  ||
                       ||||||   | |/++-+-----+++--+++-+++++++---++++++--++-+++-++-+-+++--++-+----+++-+--+-++++-\|||||| ||||| | |  ||| |  | |||    |||| || |||| | |  ||   |  ||
                       ||||||   | |||| |     |||  ||| |||||||   ||||||  || ||| || | |||  || |    ||| |/-+-++++-+++++++-+++++-+-+--+++-+--+-+++---\|||| || |||| | |  ||   |  ||
                       ||||||   |/++++-+-----+++--+++-+++++++---++++++--++-+++-++-+-+++--++-+----+++-++-+-++++-+++++++-+++++-+-+--+++-+\ | |||   ||||| || |||| | |  ||   |  ||
                       ||||||   |||||| |     |||  ||| |||||||   ||||||  || ||| || | |||/-++-+----+++-++-+-++++-+++++++-+++++-+-+--+++-++-+-+++---+++++-++\|||| | |  ||   |  ||
                       ||||||   |||||| |     |||  ||| |||||||   ||||||  || ||| || | ||||/++-+----+++-++-+-++++-+++++++-+++++-+-+--+++-++-+-+++---+++++-+++++++-+-+--++---+\ ||
                       |||||\---++++++-+-----+++--+++-+++++++---++++++--/| ||| || | ||||||| |    ||| || | |||| ||||||| ||||| | |  ||| || | |||   ||||| ||||||| | |  ||   || ||
                       |||||    |||||| |/----+++--+++-+++++++---++++++---+\||| || | ||v|||| |    ||| || | |||| ||||||| ||||| | |  ||| || | |||   ||||| ||||||| | |  ||   || ||
                       |||||    |||||| ||/---+++--+++-+++++++---++++++--\||||| || | ||||||| |    ||| || | ||||/+++++++-+++++-+-+--+++-++-+\|||   ||||| ||||||| | |  ||   || ||
                       |||||    ||||||/+++---+++--+++-+++++++--\||||||  |||||| || | ||||||| |    ||| || | |||||||||||| ||||\-+-+--+++-++-/||||   ||||| ||||||| | |  ||   || ||
                       |||||    ||||||||||   |||  ||| |||||||  |||||||  |||||| || | ||||||\-+----+++-++-+-++++++++++++-++++--+-+--+++-++--++++---+++++-++++++/ | |  ||   || ||
                       |||||   /++++++++++---+++\ ||| |||||||  |||||||  |||||| || | ||||||  |    ||| || | |||||||||||| ||||  | |  ||| ||  ||||  /+++++-++++++--+-+--++---++\||
                       |||||   |||||||||||   ||\+-+++-+++++++--+++++++--++++++-++-+-++++++--+----+++-++-+-++++++++++++-++++--/ |  ||| ||  ||||  |||||| ||||||  | |  ||   |||||
                       |||||   |||||||||||   || | ||| |||||||  |||||||  |||\++-++-+-++++++--+----+++-++-/ |||||||||||| ||||    |  ||| ||  ||||  |||||| ||||||  | |  ||   |||||
                       |||\+---+++++++++++---++-+-+/| |||||||  |||||||  ||| || || | ||||||  |    ||| ||   |||||||||||| ||||    |  ||| ||  ||||  |||||| ||||||  | |  ||   |||||
                       ||| |   ||\++++++++---++-+-+-+-+++++++--+++++++--+++-++-++-+-++++++--+----+++-++---++++++++++++-++++----+--+++-+/  ||||  |||||| ||||||  | |  ||   |||||
                       ||| |   || ||||||||   || | | | |||||||  |||||||  ||| || || | ||||||  |    ||| ||   |||||||||||| ||||    |  ||| |   ||||  |||||| ||||||  | |  ||   |||||
                       ||| |   || ||||||||   || | | | |||||||  |||||||  ||| || || | ||||||  |    ^|| ||   |||||||||||| ||||    |  ||| |   ||||  ||||\+-++++++--+-+--++---+++/|
                       ||| |   || ||||\+++---++-+-+-+-+++++++--/||||||  |\+-++-++-+-++++++--+----+++-++---++++++++++++-++++-->-+--+++-+---++++--++++-+-++++++--/ |  ||   ||| |
                       ||| |   || |||| \++---++-+-+-+-+++++++---++++++--+-+-++-++-+-++++++--+----++/ ||   |||||||||||| ||||    |  ||| |   ||||  |||| | ||||||    |  ||   ||| |
                       ||| |   || ||||  ||   \+-+-+-+-+++++/|   ||||||  | | || || | ||||||  |    ||  ||   ||\+++++++++-++++----+--+++-+---++++--+++/ | ||||||    |  ||   ||| |
                       ||| |   || ||||  ||    | | | | ||||| |   ||||||  | | || || | ||||||  |    ||  ||   || |||||||||/++++->--+--+++-+---++++--+++--+-++++++----+--++\  ||| |
                       ||| |   || ||||  ||    | | | | ||||| |   ||||||  | | || || | ||||||  |    ||  ||   || ||||||||||||||/---+--+++-+---++++--+++--+-++++++----+--+++\ ||| |
                       |||/+---++-++++--++----+-+-+-+-+++++-+---++++++--+-+\|| || | ||||||  |    ||  ||   || |||||||||||||||   |  ||| |   ||||  |||  | ||||||    |  |||| ||| |
                       |||||   || ||||  ||    | | | | |||\+-+---++++++--+-++++-++-+-++++++--+----++--++---++-+++++++++++++++---+--+++-+---+++/  |||  | ||||||    |  |||| ||| |
                       |||||   || ||||  ||    | | | | |||/+-+---++++++--+-++++-++-+-++++++--+----++--++---++-+++++++++++++++---+--+++-+---+++-\ |||  | ||||||    |  |||| ||| |
                       |||||   || ||||  ||    | | | | ||||| | /-++++++--+-++++-++-+-++++++--+----++--++---++-+++++++++++++++---+--+++-+---+++-+-+++\ | ||||||    |  |||| ||| |
                       |||||   || ||||/-++----+-+-+-+-+++++\| | ||||||  | |||| || | ||||||  |    ||  ||   || |\+++++++++++++---+--+++-+---/|| | |||| | ||||||    |  |||| ||| |
                       |||||   || ||||| ||    | | | | ||||||| | ||||||  | |||| || | ||||||  |    ||  |v   || | |||||||||||||   |  |||/+----++-+-++++-+\||||||    |  |||| ||| |
                       |||||   || ||||v ||    | | | | ||||||| | ||||||  | |||| || |/++++++--+----++--++---++-+-+++++++++++++---+--+++++----++-+\|||| ||||||||    |  |||| ||| |
                       |\+++---++-+++++-++----+-+-+-+-+++++++-+-++++++--+-++++-++-++++++++--+----++--++---++-+-+++/|||||||||   |  |||||    || |||||| ||||||||    |  |||| ||| |
                       | |||   || ||||| ||    \-+-+-+-+++++++-+-+++/||  | |||| || |||||||\--+----++--++---++-+-+++-++/||||||   |  |||||/---++-++++++-++++++++----+\ |||| ||| |
                       | |||   || ||||| ||      | | | ||||||| | ||| ||  | |||| || |||||||   |    ||  ||   || | ||| || ||\+++---+--/|||||   || |||||| ||||||||    || |||| ||| |
                       | |||   || ||||| ||      | | | ||||||| | ||| ||  | |||| || \++++++---+----++--++---++-+-++/ \+-++-+++---+---+++++---/| |||||| ||||||||    \+-++++-+++-/
                       | |||   || v|||| ||     /+-+-+-+++++++-+-+++-++--+-++++-++--++++++---+----++--++---++-+-++---+-++-+++\  |   |||||   /+-++++++-++++++++----\| |||| |||
                       | |||   || ||||| ||     || | | ||||||| | ||| ||  | |||| \+--++++++---+----++--++---++-+-++---+-++-++++--+---+++++---++-++++++-+++/||||    || |||| |||
                       | |\+---++-+++++-++-----++-+-+-+++++++-+-+++-++--+-+/||  |  |\++++---+----++--++---++-+-++---+-++-++++--+---+++++---++-++++++-+++-++++----++-++++-/||
                       | | |   || ||||| ||     || | | ||||||| | ||| ||  | | ||  |  | ||||   |    ||  |\---++-+-++---+-++-++++--+---+++++---++-+++/|| ||| ||||    || ||||  ||
                       | | |   || ||||| ||   /-++-+-+-+++++++-+-+++-++--+-+-++--+-\| ||||   |/---++--+----++-+-++---+-++-++++-\|   |||||   || ||| || ||| ||||    || ||||  ||
                       | | |   || ||||| ||   | || | | ||||\++-+-+++-++--+-+-++--+-++-++++---++---++--+----++-+-++---+-++-++++-++---+++++---+/ ||| || ||| ||||    || ||||  ||
                       |/+-+---++-+++++-++---+-++-+-+-++++-++-+-+++-++--+-+-++--+-++-++++\  ||   ||  |    || | ||   | || |||| ||   |||||   |  ||| || ||| ||||    || ||||  ||
                       ||| |   || ||||| ||   | || | | |||| || | ||| ||  | | ||  |/++-+++++--++---++-\|    || | ||   | || |||| ||   |||||   |  ||| || ||| ||||    || ||||  ||
                       ||| |   || ||||| ||   | || | | |||| || | ||| ||  | | ||  ||||/+++++--++---++-++----++-+-++---+-++-++++-++-\ |||||   |  ||| || ||| ||||    || ||||  ||
                       ||| |   || \++++-++---+-++-+-+-++++-++-+-++/ ||  | | ||/-++++++++++--++---++-++----++-+-++--\| || |||| || | |||||   |  ||| || ||| ||||    || ||||  ||
                       ||| |   ||  |||| ||   | || | | |||| || | ||  |\--+-+-+++-++++++++++--++---++-+/    || | ||  || || |||| || | ||||\---+--+++-++-+++-++++----+/ ||||  ||
                       ||| |   ||  |||| ||   | || | | |||| || | ||  |   | | ||| ||||||||||  ||   || |     || | ||  || || |||| || | ||||    |  ||| || ||| ||||    |  ||||  ||
                       ||| |   ||  |\++-++---+-++-+-+-++++-++-+-++--+---+-+-+++-++++++++++--++---+/ |     || | ||  || || |||| || | ||||    |  ||| || ||| ||||    |  ||||  ||
                       ||| |   ||  | || ||   | || | | |||| || | ||  |   | | ||| |||||\++++--++---+--+-----++-+-++--++-++-++++-++-+-++++----+--+++-++-++/ ||||    |  ||||  ||
                       ||| |   ||  | || ||   | || | | |||| || | ||  |   | | ||| ||||| ||||  ||   |  |     || | ||  || |\-++++-++-+-++++----+--+++-++-/|  ||||/---+--++++--++\
                       ||| |   ||  | || || /-+-++-+-+-++++-++-+-++--+---+-+-+++-+++++-++++--++---+--+-----++-+-++--++-+--++++-++-+-++++-\  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | || || | | || | | |\++-++-+-++--+---+-+-+++-+++++-++++--++---+--+-----++-+-++--++-+--+/|| || | |||| |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | || || | | || |/+-+-++-++-+-++--+---+-+-+++-+++++-++++--++---+\ |     || | ||  || |  | || || | |||| |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | || || | | || ||| | || || | ||  |   | | ||| ||||| ||||  ||   || |     || | ||  || |  | || || | |||| |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | || || | |/++-+++-+-++\|| | ||  |   | | ||| |\+++-++++--++---++-/     || | ||  || |  | || || | ||v| |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | || || | ||\+-+++-+-+++++-+-++--+---+-+-+++-+-+++-++++--++---++-------++-+-++--++-+--+-+/ || | |||| |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | || || | || | ||| \-+++++-+-++--+---+-+-+++-+-+++-++++--++---/|       || | ||  || |  | |  || | |||| |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | |\-++-+-++-+-+++---+++/| | \+--+---+-+-+++-+-+++-++++--++----+-------++-+-++--++-+--+-+--++-+-+++/ |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | ^  || | || | |||   ||| | |  |  |   | | ||| | ||| ||||  ||    |       || | ||  || |  | |  || | |||  |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | |  || | || | |||   ||| | |  |  |   | | ||| | ||| ||||  \+----+-------++-+-++--++-+--+-+--++-+-/||  |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | |  || | || | ||| /-+++-+-+--+--+---+-+-+++-+-+++\||||   |    |       || | ||  || |  | |  || |  ||  |  |  ||| ||  |  |||||   |  ||||  |||
                       ||| |   ||  | |  || | || | |||/+-+++-+-+--+\ |   | | ||| | ||||||||   |    |       || | ||  || |  | |  || |  ||  |  |  ||| \+--+--+++++---+--+/||  |||
                       ||| | /-++--+-+--++-+-++-+-+++++\||| | |  || |   | | ||| | ||||||||   |    |       || | ||  || |  | |  || |  ||  |  |  |||  |  |  |||||   |  | ||  |||
                       ||| | | ||  | |  || | || | ||||||\++-+-+--++-+---+-+-+++-+-++++++++---+----+-------++-+-++--++-+--+-+--++-+--++--+--+--+++--+--+--+++/|   |  | ||  |||
                       ||\-+-+-++--+-+--++-+-++-+-++++++-++-+-+--++-/   | | ||| | ||||||||   |    |       || | ||  || |  | |  || |/-++--+--+--+++--+--+--+++-+---+\ | ||  |||
                       ||  | | ||  | |  || | || | |||||| \+-+-+--++-----+-+-+++-+-++++++++---+----+-------++-+-++--++-+--+-+--++-++-++--+--+--/||  |  |  ||| |   || | ||  |||
                       ||  | | ||  | |  || | || | ||||||  | | |  ||     | | ||| | ||||||||   |  /-+-------++-+-++--++-+\ \-+--++-++-++--+--+---++--+--+--++/ |   || | ||  |||
                       ||  | | ||  | |  || | || | ||||||  | | |  ||     | | ||| | ||||||||   |  | |       || | ||  || ||   |  || || ||  |  |   ||  |  |  ||  |   || | ||  |||
                       ||  | | ||  | |  || | || | ||||||  | | |  ||     | | ||| | ||||||||   \--+-+-------++-+-++--++-++---+--/| || ||  |  |   ||  |  |  ||  |   || | ||  |||
                       ||  | | ||  | |  || | || | ||||||  | | |  ||     | | ||| | ||||||||      | |       || | ||  || ||   |   | || ||  |  |   ||  |  |  ||  |   || | ||  |||
                       ||  |/+-++--+-+--++-+-++-+-++++++--+-+-+--++-----+-+\||| | ||||||||      | |       || | ||  || ||   |   | || ||  |  |   ||  |  |  ||  \---++-+-++--++/
                       ||  ||| ||  | \--++-+-++-+-/|||\+--+-+-+--++-----+-+++++-+-+++/||||      | |       || | ||/-++-++---+---+-++-++--+--+---++--+\ |  ||      || | ||  ||
                       ||  ||| ||  |    || | || |  ||| |  | | |  \+-----+-+++++-+-+++-++++------+-+-------+/ | ||| || ||   |   | || ||  |  |   ||  || |  ||      || | ||  ||
                       ||  ||| ||  |    || | || |  ||| |  | | |   |     | ||||| | ||| ||||      | |       |  | ||| || ||   |   \-++-++--+--+---++--++-+--+/      || | ||  ||
                       ||  ||| ||  |    || | || |  ||| |  | | |   |     | ||||| | ||| ||||      | |       |  | ||| || ||   |     || ||  |  |   ||  || |  |       || | ||  ||
                       ||  ||| ||  |    || | || |  |\+-+--+-+-+---+-----+-+++++-+-+++-++++------+-+-------+--+-+++-++-++---+-----++-/|  |  |   ||  || |  |       || | ||  ||
                       ||  ||| ||  |    || | || |  | | |  | | |   |     | ||||| | ||| ||||      | |       |  | ||| |v ||   |     ||  |  |  |   ||  || |  |       || | ||  ||
                       ||  ||| ||  |    || | || |  | | |  | | |   |     | ||||| \-+++-++++------+-+-------+--+-+++-+/ ||   |     ||  |  |  |   ||  || |  |       || | ||  ||
                       ||  \++-++--+----++-+-++-+--+-+-+--+-+-+---+-----+-+++++---+++-++++------+-+-------+--/ ||| |  \+---+-----++--+--+--+---++--++-+--+-------++-+-/|  ||
                       ||   || ||  |    || | |\-+--+-+-+--/ | |   |     | |||\+---+++-++++------+-+-------+----+++-+---+---+-----++--+--+--+---++--++-+--+-------++-/  |  ||
                       ||   |\-++--+----++-+-+--+--+-+-/    | |   |     | ||| |   ||| ||||      | |       |    ||| |   |   |     |\--+--+--+---++--++-+--+-------+/    |  ||
                       ||   |  ||  |    || | |  |  | |      | |   |     | ||| |   ||| ||||      | |       |    ||| |   |   |     |   |  |  |   ||  || |  |       |     |  ||
                       |\---+--++--+----++-+-+--+--+-+------+-+---+-----+-+++-+---+++-+++/      | |       |    ||\-+---+---+-----+---+--+--+---++--+/ |  |       |     |  ||
                       |    |  ||  |    |\-+-+--+--+-+------+-+---+-----/ ||| |   ||| |||       \-+-------+----++--+---/   \-----+---+--+--+---++--+--+--+-------+-----/  ||
                       |    |  ||  |    |  | |  |  | |      | |   |       ||| |   ||| |\+---------+-------+----++--+-------------+---+--+--+---++--+--+--/       |        ||
                       |    |  ||  |    |  | \--+--+-+------+-+---+-------+++-+---/|| \-+---------+-------+----+/  |             |   |  |  |   ||  |  |          |        ||
                       |    |  |\--+----+--+----+--+-+------+-+---+-------+++-+----++---+---------+-------/    |   |             |   \--+--+---++--+--/          |        ||
                       |    |  \---+----+--+----/  | |      | |   |       ||| |    \+---+---------+------------+---+-------------+------+--+---/|  |             |        ||
                       |    |      |    |  |       | |      \-+---+-------++/ |     |   |         |            |   |             |      |  |    \--+-------------+--------+/
                       |    |      |    |  |       | |        |   |       ||  |     \---+---------+------------+---+-------------/      |  |       |             |        |
                       |    |      \----+--+-------+-+--------+---+-------++--+---------+---------+------------/   |                    |  |       |             |        |
                       |    |           |  |       | \--------+---/       ||  |         |         |                |                    |  |       |             |        |
                       |    |           |  |       \----------+-----------++--+---------+---------/                |                    |  \-------+-------------/        |
                       |    |           |  \------------------+-----------++--+---------+--------------------------+--------------------/          |                      |
                       |    \-----------+---------------------+-----------+/  |         \--------------------------+-------------------------------+----------------------/
                       |                |                     \-----------+---+------------------------------------+-------------------------------/
                       |                \---------------------------------/   \------------------------------------/                                                          """.stripMargin
}