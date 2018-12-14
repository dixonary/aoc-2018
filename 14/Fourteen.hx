package;

class Fourteen {
    
    static public function main() {

        // PART 1
        var num = 554401;
        var cnum = 0;
        var recipes = [3,7];
        var elf1 = 0;
        var elf2 = 1;

        while(recipes.length <= num+10) {
            var newR = split(recipes[elf1] + recipes[elf2]); 
            for(e in newR) recipes.push(e);
            elf1 += recipes[elf1]+1;
            elf1 %= recipes.length;
            elf2 += recipes[elf2]+1;
            elf2 %= recipes.length;
        }
        for(i in num ... num + 10) {
            Sys.print(recipes[i]);
        }
        Sys.println("");


        // PART 2
        var cnum = 0;
        var recipes = [3,7];
        var elf1 = 0;
        var elf2 = 1;
        var nsplit = split(num);
        while(true) {
            cnum++;
            var newR = split(recipes[elf1] + recipes[elf2]); 
            for(e in newR) recipes.push(e);
            elf1 += recipes[elf1]+1;
            elf1 %= recipes.length;
            elf2 += recipes[elf2]+1;
            elf2 %= recipes.length;

            if(same(recipes.slice(-nsplit.length-1,-1), nsplit)) {
                nsplit.push(recipes[recipes.length-1]);
                break;
            }
            if(same(recipes.slice(-nsplit.length), nsplit)) {
                break;
            }
        }
        Sys.println(recipes.length-nsplit.length);
    }


    static public function same(s1:Array<Int>, s2:Array<Int>) {
        if (s1.length != s2.length) return false;
        for(i in 0 ... s1.length) {
            if(s1[i] != s2[i]) return false;
        }
        return true;
    }


    static public function split(str:Int):Array<Int> {
        return (""+str).split("").map(Std.parseInt);       
    }

}
