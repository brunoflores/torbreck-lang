let f = (lambda x:{x:Bool}. x.x) in
let record1 = {x = true, y = false} in
let record2 = {y = false} in 
f record2;
