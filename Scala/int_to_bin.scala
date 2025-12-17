@main 
def to_bin_rec(num: Int) : String = 
    num match
        case 0 => "0"
        case _ =>  to_bin_rec(num / 2)+(num % 2).toString 
    

//def to_bin_tail()
