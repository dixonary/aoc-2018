package;
import haxe.Int64;
using Lambda;

class Nine {

    static public function main () {
        var players = 424;
        var marble  = 7148200;
        var scores = [for (i in 0...players) Int64.make(0,0)];
        var player = 0;
        
        var coil = new Coil(0);

        for(i in 1...marble+1) {
            if(i % 23 == 0) {
                var sevenBack = coil.move(-7);
                scores[player] += i + sevenBack.val;
                coil = sevenBack.delete();
            }
            else {
                coil = coil.move(1).insert(i);
            }
            player += 1;
            player %= scores.length;
        }   

        var max = scores.fold(function(a, b) return a > b ? a : b, 0);
        Sys.println (max);
    }
}


@:generic
class Coil {
    var left:Coil;
    public var right:Coil;
    public var val:Int;

    public function new(val:Int) {
        left = this;
        right = this;
        this.val = val;
    }

    public function toString() {
        var vals = [this];
        var k = right;
        while (k != this) {
            vals.push(k);
            k = k.right;    
        } 
        return "" + vals.map(function(c){return c.val;});
    }

    public function insert(val:Int):Coil {
        var c = new Coil(val);

        c.right = right; 
        right.left = c;

        c.left = this;
        right = c;

        return c;
    }

    public function delete():Coil {
        left.right = right;
        right.left = left;
        return right;
    }

    public function move(n:Int):Coil {
        if(n == 0) return this;
        if(n<0) return left.move(n+1);
        if(n>0) return right.move(n-1); 
        return this;
    }   

}
