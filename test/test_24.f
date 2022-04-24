-- Variants and Case

PhysicalAddr = { firstlast: String, addr: String };
VirtualAddr = { name: String, email: String };

pa = { firstlast = "Bruno Flores", addr = "Main St." } as PhysicalAddr;

Addr = <physical: PhysicalAddr, virtual: VirtualAddr>;

a = <physical=pa> as Addr;

getName = lambda a: Addr.
  case a of
      <physical=x> ==> x.firstlast
    | <virtual=y> ==> y.name;

getName a;
