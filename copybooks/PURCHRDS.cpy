       01 PURCHASE-ORDER.
          05 ORDER-DETAILS.
             10 PO-NUMBER           PIC        X(06) VALUE SPACES.
             10 BUYER-CODE          PIC        X(03) VALUE SPACES.
             10 QUANTITY            PIC        S9(8) COMP
                                                  VALUE ZEROS.
             10 UNIT-PRICE          PIC S9(7)V99 COMP-3
                                                  VALUE ZEROS.
             10 ORDER-DATE          PIC        X(08) VALUE SPACES.
             10 DELIVERY-DATE       PIC    X(08)     VALUE SPACES.
          05 PART-NUMBER         PIC    X(11)     VALUE SPACES.
          05 SUPPLIER-CODE       PIC    X(5)      VALUE SPACES.
