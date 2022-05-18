CREATE TABLE DEPT (
    DEPTNO  INTEGER NOT NULL PRIMARY KEY,
    DNAME   VARCHAR(14),
    LOC     VARCHAR(13)
);

COMMIT;

CREATE TABLE EMP (
    EMPNO     INTEGER NOT NULL PRIMARY KEY,
    ENAME     VARCHAR(10),
    JOB       VARCHAR(9),
    MGR       INTEGER,
    HIREDATE  TIMESTAMP,
    SAL       NUMERIC(7, 2),
    COMM      NUMERIC(7, 2),
    DEPTNO    INTEGER REFERENCES DEPT (DEPTNO)
);

CREATE TABLE UniDAC_BLOB (
  ID integer NOT NULL PRIMARY KEY,
  Title VARCHAR(30),
  Picture BLOB
);

CREATE TABLE UniDAC_TEXT (
  ID INTEGER NOT NULL PRIMARY KEY,
  Title VARCHAR(30),
  TextField BLOB SUB_TYPE text
);

CREATE TABLE UniDAC_Loaded (
  Intg INTEGER,
  Dbl  DOUBLE PRECISION,
  Str  VARCHAR(50),
  Dat  DATE
);

CREATE TABLE CRGRID_TEST (
  Id NUMERIC(4) NOT NULL PRIMARY KEY,
  Name VARCHAR(10),
  Country VARCHAR(30),
  City VARCHAR(30),
  Street VARCHAR(30),
  BirthDate TIMESTAMP,
  Job VARCHAR(9),
  Hiredate TIMESTAMP,
  Sal NUMERIC(7, 2),
  Remarks BLOB SUB_TYPE TEXT
);

SET TERM ^;

CREATE PROCEDURE SEL_FROM_EMP 
RETURNS (
    EMPNO INTEGER,
    ENAME VARCHAR(10),
    JOB VARCHAR(9),
    MGR INTEGER,
    SAL INTEGER,
    COMM INTEGER,
    DEPTNO INTEGER)
AS
BEGIN
  FOR  SELECT EMPNO, ENAME, JOB, MGR, SAL, COMM, DEPTNO FROM emp
    INTO :EMPNO, :ENAME, :JOB, :MGR, :SAL, :COMM, :DEPTNO
  DO
    suspend;
END^

SET TERM ;^

COMMIT;

INSERT INTO DEPT VALUES
  (10,'ACCOUNTING','NEW YORK');
INSERT INTO DEPT VALUES
  (20,'RESEARCH','DALLAS');
INSERT INTO DEPT VALUES
  (30,'SALES','CHICAGO');
INSERT INTO DEPT VALUES
  (40,'OPERATIONS','BOSTON');
INSERT INTO EMP VALUES
  (7369,'SMITH','CLERK',7902,'17.12.1980',800,NULL,20);
INSERT INTO EMP VALUES
  (7499,'ALLEN','SALESMAN',7698,'20.02.1981',1600,300,30);
INSERT INTO EMP VALUES
  (7521,'WARD','SALESMAN',7698,'22.02.1981',1250,500,30);
INSERT INTO EMP VALUES
  (7566,'JONES','MANAGER',7839,'02.04.1981',2975,NULL,20);
INSERT INTO EMP VALUES
  (7654,'MARTIN','SALESMAN',7698,'28.09.1981',1250,1400,30);
INSERT INTO EMP VALUES
  (7698,'BLAKE','MANAGER',7839,'01.05.1981',2850,NULL,30);
INSERT INTO EMP VALUES
  (7782,'CLARK','MANAGER',7839,'09.06.1981',2450,NULL,10);
INSERT INTO EMP VALUES
  (7788,'SCOTT','ANALYST',7566,'13.07.87',3000,NULL,20);
INSERT INTO EMP VALUES
  (7839,'KING','PRESIDENT',NULL,'17.11.1981',5000,NULL,10);
INSERT INTO EMP VALUES
  (7844,'TURNER','SALESMAN',7698,'08.09.1981',1500,0,30);
INSERT INTO EMP VALUES
  (7876,'ADAMS','CLERK',7788,'13.07.87',1100,NULL,20);
INSERT INTO EMP VALUES
  (7900,'JAMES','CLERK',7698,'03.12.1981',950,NULL,30);
INSERT INTO EMP VALUES
  (7902,'FORD','ANALYST',7566,'03.12.1981',3000,NULL,20);
INSERT INTO EMP VALUES
  (7934,'MILLER','CLERK',7782,'23.01.1982',1300,NULL,10);

INSERT INTO CRGRID_TEST (Id, Name, Country, City, Street, BirthDate, Job, HireDate, Sal) VALUES
  (5001, 'SMITH', 'ENGLAND', 'LONDON', 'BOND st.', '12.10.63', 'CLERK', '17.12.80', 800);

INSERT INTO CRGRID_TEST (Id, Name, Country, City, Street, BirthDate, Job, HireDate, Sal) VALUES
  (5002, 'ALLEN', 'ENGLAND', 'LONDON', 'BAKER st.', '04.03.61', 'SALESMAN', '20.02.81', 1600);

INSERT INTO CRGRID_TEST (Id, Name, Country, City, Street, BirthDate, Job, HireDate, Sal) VALUES
  (5003, 'MARTIN', 'FRANCE', 'LION', 'WEAVER st.', '23.01.57', 'MANAGER', '02.04.81', 2900);

COMMIT;
