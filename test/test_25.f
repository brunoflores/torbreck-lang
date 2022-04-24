-- Single-field variants for type-safety.

DollarAmount = <dollars: Float>;
EuroAmount = <euros: Float>;

dollars2euros =
  lambda d: DollarAmount.
    case d of <dollars = x> ==>
      <euros = timesfloat x 1.1325> as EuroAmount;

euros2dollars =
  lambda e: EuroAmount.
    case e of <euros = x> ==>
      <dollars = timesfloat x 0.883> as DollarAmount;

-- Do some calculation.
mybankbalance = <dollars = 39.50> as DollarAmount;
euros2dollars (dollars2euros mybankbalance);
