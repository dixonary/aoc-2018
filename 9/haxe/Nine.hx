package;

import haxe.Int64;
using Lambda;

class Nine {
    static public function main () {
        var players = 424;
        var marble  = 7148200;

        var player = 0;
        var coil = new Coil(0);
        var scores = [for (i in 0...players) Int64.make(0,0)];

        for(i in 1...marble+1) {
            if(i % 23 == 0) {
                coil = coil.move(-7);
                scores[player] += i + coil.val;
                coil = coil.delete();
            }
            else {
                coil = coil.move(1).insert(i);
            }
            player = (player + 1) % scores.length;
        }   

        var max = scores.fold(function(a,b) return a > b ? a : b, 0);
        Sys.println(max); 
    }
}

@:generic
class Coil<T> {
    public var left :Coil<T>;
    public var right:Coil<T>;
    public var val  :T;

    public function new(val:T, ?l:Coil<T>, ?r:Coil<T>) {
        left  =  l == null ? this : l;
        right =  r == null ? this : r;
        this.val = val;
    }

    public function insert(val:T):Coil<T> {
        var c = new Coil<T>(val, this, right);
        right.left = c;
        right = c;
        return c;
    }

    public function delete():Coil<T> {
        left.right = right;
        right.left = left;
        return right;
    }

    public function move(n:Int):Coil<T> {
        if(n == 0) return this;
        if(n<0) return left.move(n+1);
        if(n>0) return right.move(n-1); 
        return this;
    }   
}
