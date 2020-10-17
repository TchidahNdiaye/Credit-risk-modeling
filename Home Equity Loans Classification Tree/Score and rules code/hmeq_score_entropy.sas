****************************************************************;
******        HP TREE (PROC HPSPLIT) SCORING CODE        ******;
****************************************************************;
 
******              LABELS FOR NEW VARIABLES              ******;
LABEL _Node_ = 'Node number';
LABEL _Leaf_ = 'Leaf number';
LABEL _WARN_ = 'Warnings';
LABEL P_BAD0 = 'Predicted: BAD=0';
LABEL P_BAD1 = 'Predicted: BAD=1';
LABEL V_BAD0 = 'Validated: BAD=0';
LABEL V_BAD1 = 'Validated: BAD=1';
 
 _WARN_ = ' ';
 
******      TEMPORARY VARIABLES FOR FORMATTED VALUES      ******;
 
******             ASSIGN OBSERVATION TO NODE             ******;
IF NOT MISSING(DEBTINC) AND ((DEBTINC >= 43.84338265478))
 THEN DO;
  IF NOT MISSING(CLAGE) AND ((CLAGE >= 222.358612846148))
   THEN DO;
    IF NOT MISSING(DEBTINC) AND ((DEBTINC >= 46.710400330072))
     THEN DO;
      _Node_ = 10;
      _Leaf_ = 3;
      P_BAD0 = 0;
      P_BAD1 = 1;
      V_BAD0 = 0;
      V_BAD1 = 1;
    END;
    ELSE DO;
      _Node_ = 9;
      _Leaf_ = 2;
      P_BAD0 = 0.81818182;
      P_BAD1 = 0.18181818;
      V_BAD0 = 1;
      V_BAD1 = 0;
    END;
END;
ELSE DO;
  _Node_ = 5;
  _Leaf_ = 1;
  P_BAD0 = 0;
  P_BAD1 = 1;
  V_BAD0 = 0.1;
  V_BAD1 = 0.9;
END;
END;
ELSE DO;
IF NOT MISSING(DELINQ) AND ((DELINQ >= 4.100000000000001))
 THEN DO;
  _Node_ = 4;
  _Leaf_ = 0;
  P_BAD0 = 0;
  P_BAD1 = 1;
  V_BAD0 = 0;
  V_BAD1 = 1;
END;
ELSE DO;
  IF NOT MISSING(DEROG) AND ((DEROG >= 1.1))
   THEN DO;
    IF NOT MISSING(CLNO) AND ((CLNO >= 30.08))
     THEN DO;
      _Node_ = 14;
      _Leaf_ = 6;
      P_BAD0 = 0;
      P_BAD1 = 1;
      V_BAD0 = 0;
      V_BAD1 = 1;
    END;
    ELSE DO;
      _Node_ = 13;
      _Leaf_ = 5;
      P_BAD0 = 0.73684211;
      P_BAD1 = 0.26315789;
      V_BAD0 = 0.85714286;
      V_BAD1 = 0.14285714;
    END;
  END;
  ELSE DO;
    IF NOT MISSING(CLAGE) AND ((CLAGE < 175.64873886818))
     THEN DO;
      IF NOT MISSING(LOAN) AND ((LOAN < 6110))
       THEN DO;
        IF NOT MISSING(MORTDUE) AND ((MORTDUE < 44509.6))
         THEN DO;
          _Node_ = 17;
          _Leaf_ = 8;
          P_BAD0 = 0.1;
          P_BAD1 = 0.9;
          V_BAD0 = 0.33333333;
          V_BAD1 = 0.66666667;
        END;
        ELSE DO;
          _Node_ = 18;
          _Leaf_ = 9;
          P_BAD0 = 0.87096774;
          P_BAD1 = 0.12903226;
          V_BAD0 = 0.875;
          V_BAD1 = 0.125;
        END;
      END;
      ELSE DO;
        _Node_ = 16;
        _Leaf_ = 7;
        P_BAD0 = 0.92675781;
        P_BAD1 = 0.073242188;
        V_BAD0 = 0.91666667;
        V_BAD1 = 0.083333333;
      END;
    END;
    ELSE DO;
      _Node_ = 12;
      _Leaf_ = 4;
      P_BAD0 = 0.96810345;
      P_BAD1 = 0.031896552;
      V_BAD0 = 0.96909492;
      V_BAD1 = 0.030905077;
    END;
  END;
END;
END;
****************************************************************;
******     END OF HP TREE (PROC HPSPLIT) SCORING CODE    ******;
****************************************************************;
