CREATE USER JFVARGAS IDENTIFIED BY "123";
GRANT CREATE SESSION TO JFVARGAS;
GRANT CREATE TYPE TO JFVARGAS;
GRANT CREATE PROCEDURE TO JFVARGAS;
GRANT CREATE TABLE TO JFVARGAS;
CREATE USER HANFN IDENTIFIED BY "123";
GRANT CREATE TABLE TO HANFN;
GRANT CREATE SESSION TO HANFN;
GRANT UNLIMITED TABLESPACE TO HANFN;
ALTER USER JFVARGAS DEFAULT TABLESPACE HUB;
ALTER USER HANFN DEFAULT TABLESPACE HUB;
GRANT UNLIMITED TABLESPACE TO JFVARGAS;
grant create any table to JFVARGAS;
grant create any INDEX to JFVARGAS;
grant ALTER any table to JFVARGAS;
grant create DATABASE LINK to JFVARGAS;
grant INSERT any TABLE to JFVARGAS;
grant SELECT any TABLE to JFVARGAS;
grant UPDATE any TABLE to JFVARGAS;
grant delete any TABLE to JFVARGAS;
grant DROP any TABLE to JFVARGAS;
grant create ANY DIRECTORY to JFVARGAS;
GRANT CREATE ANY SEQUENCE TO JFVARGAS;
GRANT DATAPUMP_EXP_FULL_DATABASE TO JFVARGAS;

DROP TABLE SPANISH_CHARS;
CREATE TABLE SPANISH_CHARS AS 
SELECT
ROWNUM COD,
CHR(A) B
FROM
(
SELECT *
FROM
(
SELECT ROWNUM A
FROM
ALL_TAB_COLS
WHERE ROWNUM<=256 
)
WHERE 
A BETWEEN 48 AND 57 OR
A BETWEEN 65 AND 90 OR 
A BETWEEN 97 AND 122 OR 
A IN (193,201,205,209,211,218,220,225,233,237,243,250,252,241,32,46,44)
)A;

DROP FUNCTION "GC_SPA" ;
CREATE OR REPLACE FUNCTION "GC_SPA" 
RETURN VARCHAR
AS
C VARCHAR(1);
BEGIN
SELECT B INTO C
FROM
SPANISH_CHARS
WHERE 
COD=(SELECT CEIL(DBMS_RANDOM.VALUE(0,(SELECT COUNT(*) FROM SPANISH_CHARS))) FROM DUAL);
RETURN C;
END GC_SPA;

/

DROP FUNCTION "GV5_SPA";
CREATE OR REPLACE FUNCTION "GV5_SPA" (N NUMBER)
RETURN VARCHAR2
AS
C VARCHAR(1000);
I NUMBER;
BEGIN
FOR I IN 0..N
LOOP
SELECT C||GC_SPA INTO C FROM DUAL;
END LOOP;
RETURN C;
END GV5_SPA;

/

DROP FUNCTION "GENERATE_VARCHAR";
CREATE OR REPLACE FUNCTION "GENERATE_VARCHAR" (N IN NUMBER)
  RETURN VARCHAR2
IS
  S VARCHAR2(1000);
BEGIN
  SELECT DBMS_RANDOM.STRING('X',N) INTO S FROM DUAL;
  RETURN S;
EXCEPTION
  WHEN OTHERS THEN
    RETURN NULL;
END GENERATE_VARCHAR;
/

DROP TYPE "VAR_ARRAY";
CREATE OR REPLACE TYPE "VAR_ARRAY" AS TABLE OF VARCHAR2(1000);
/
DROP FUNCTION "GENERATE_VARCHAR_SET";
CREATE OR REPLACE FUNCTION "GENERATE_VARCHAR_SET" (P1 VAR_ARRAY)
  RETURN VARCHAR2
IS
N NUMBER(5);
I NUMBER(5);
BEGIN
  N:=P1.LAST;
  SELECT DBMS_RANDOM.VALUE(1,N) INTO I FROM DUAL;
  RETURN P1(I);
--EXCEPTION
--  WHEN OTHERS THEN
--    RETURN NULL;
END GENERATE_VARCHAR_SET;
/

create type dyn_pipeline as object
   (
     atype anytype,
 
     static function ODCITableDescribe(rtype out anytype,
                                       stmt  in  varchar2)
       return number,
 
     static function ODCITablePrepare(sctx      out dyn_pipeline,
                                      tf_info   in  sys.ODCITabfuncinfo,
                                      stmt      in  varchar2)
       return number,
 
     static function ODCITableStart(sctx  in out dyn_pipeline,
                                    stmt  in     varchar2)
       return number,
 
     member function ODCITablefetch(self  in out dyn_pipeline,
                                    nrows in     number,
                                   rws   out    anydataset)
       return number,
 
     member function ODCITableClose(self in dyn_pipeline)
       return number
   );
   /
   
create package pkg_pipeline
   as
 
     /*
      * Global Types
      */
     -- Describe array.
     type dynamic_sql_rec is record(cursor    integer,
                                    column_cnt  pls_integer,
                                    description dbms_sql.desc_tab2,
                                    execute     integer);
     -- Meta data for the ANYTYPE.
     type anytype_metadata_rec is record(precision pls_integer,
                                         scale     pls_integer,
                                         length    pls_integer,
                                         csid      pls_integer,
                                         csfrm     pls_integer,
                                         schema    varchar2(30),
                                         type      anytype,
                                         name      varchar2(30),
                                         version   varchar2(30),
                                         attr_cnt  pls_integer,
                                         attr_type anytype,
                                         attr_name varchar2(128),
                                         typecode  pls_integer);
 
 
     /*
      * Global Variables
      */
     -- SQL descriptor.
     r_sql dynamic_sql_rec;
 
     /*
      * function will run the given SQL
      */
     function querydb(p_stmt in varchar2)
       return anydataset pipelined using dyn_pipeline;
   
 
   end pkg_pipeline;
   /
   
   create type body dyn_pipeline
  as

    /*
     * DESC step. this will be called at hard parse and will create
     * a physical type in the DB Schema based on the select columns.
     */
    static function ODCITableDescribe(rtype out anytype,
                                      stmt  in  varchar2)
      return number
    is

      /* Variables */
      -- Type to hold the dbms_sql info (description)
      r_sql   pkg_pipeline.dynamic_sql_rec;
      -- Type to create (has all the columns) of the sql query.
      t_anyt  anytype;
      -- SQL query that will be made up from the 2 passed in queries.
      v_sql   varchar2(32767);

    begin

      /*
       * Parse the SQL and describe its format and structure.
       */
      v_sql := replace(stmt, ';', null);

      --  open, parse and discover all info about this SQL.
      r_sql.cursor := dbms_sql.open_cursor;
      dbms_sql.parse( r_sql.cursor, v_sql, dbms_sql.native );
      dbms_sql.describe_columns2( r_sql.cursor, r_sql.column_cnt, r_sql.description );
      dbms_sql.close_cursor( r_sql.cursor );

      -- Start to create the physical type.
      anytype.BeginCreate( DBMS_TYPES.TYPECODE_OBJECT, t_anyt );

      -- Loop through each attribute and add to the type.
      for i in 1 .. r_sql.column_cnt
      loop

        t_anyt.AddAttr(r_sql.description(i).col_name,
                       case
                         when r_sql.description(i).col_type in (1,96,11,208)
                         then dbms_types.typecode_varchar2
                         when r_sql.description(i).col_type = 2
                         then dbms_types.typecode_number
                         when r_sql.description(i).col_type in (8,112)
                         then dbms_types.typecode_clob
                         when r_sql.description(i).col_type = 12
                         then dbms_types.typecode_date
                         when r_sql.description(i).col_type = 23
                         then dbms_types.typecode_raw
                         when r_sql.description(i).col_type = 180
                         then dbms_types.typecode_timestamp
                         when r_sql.description(i).col_type = 181
                         then dbms_types.typecode_timestamp_tz
                         when r_sql.description(i).col_type = 182
                         then dbms_types.typecode_interval_ym
                         when r_sql.description(i).col_type = 183
                         then dbms_types.typecode_interval_ds
                         when r_sql.description(i).col_type = 231
                         then dbms_types.typecode_timestamp_ltz
                       end,
                       r_sql.description(i).col_precision,
                       r_sql.description(i).col_scale,
                       r_sql.description(i).col_max_len,
                       r_sql.description(i).col_charsetid,
                       r_sql.description(i).col_charsetform );
      end loop;

      t_anyt.EndCreate;

      -- set the output type to our built type.
      ANYTYPE.BeginCreate(dbms_types.TYPECODE_TABLE, rtype);
      rtype.SetInfo(null, null, null, null, null, t_anyt,
                    dbms_types.TYPECODE_OBJECT, 0);
      rtype.EndCreate();

      return ODCIConst.Success;

    end ODCITableDescribe;


    /*
     * PREPARE step. Initialise our type.
     */
    static function ODCITableprepare(sctx      out dyn_pipeline,
                                     tf_info   in  sys.ODCITabfuncinfo,
                                     stmt      in  varchar2)
      return number
    is

      /* Variables */
      -- Meta data.
      r_meta   pkg_pipeline.anytype_metadata_rec;

    begin

      r_meta.typecode := tf_info.rettype.getattreleminfo(
                           1, r_meta.precision, r_meta.scale, r_meta.length,
                           r_meta.csid, r_meta.csfrm, r_meta.type, r_meta.name
                         );

      sctx := dyn_pipeline(r_meta.type);
      return odciconst.success;

    end;


    /*
     * START step. this is where we execute the cursor prior to fetching from it.
     */
    static function ODCITablestart(sctx  in out dyn_pipeline,
                                   stmt  in     varchar2)
      return number
    is

      /* Variables */
      r_meta pkg_pipeline.anytype_metadata_rec;
      v_sql varchar2(32767);
    begin

      v_sql := replace(stmt, ';', null);
      pkg_pipeline.r_sql.cursor := dbms_sql.open_cursor;
      dbms_sql.parse(pkg_pipeline.r_sql.cursor, v_sql, dbms_sql.native);
      dbms_sql.describe_columns2(pkg_pipeline.r_sql.cursor,
                                 pkg_pipeline.r_sql.column_cnt,
                                 pkg_pipeline.r_sql.description);

      -- define all the columns found to let Oracle know the datatypes.
      for i in 1..pkg_pipeline.r_sql.column_cnt
      loop

        r_meta.typecode := sctx.atype.GetAttrElemInfo(
                             i, r_meta.precision, r_meta.scale, r_meta.length,
                             r_meta.csid, r_meta.csfrm, r_meta.type, r_meta.name
                           );

        case r_meta.typecode
          when dbms_types.typecode_varchar2
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, '', 32767);
          when dbms_types.typecode_number
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as number));
          when dbms_types.typecode_date
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as date));
          when dbms_types.typecode_raw
          then
            dbms_sql.define_column_raw(pkg_pipeline.r_sql.cursor, i, cast(null as raw), r_meta.length);
          when dbms_types.typecode_timestamp
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as timestamp));
          when dbms_types.typecode_timestamp_tz
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as timestamp with time zone));
          when dbms_types.typecode_timestamp_ltz
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as timestamp with local time zone));
          when dbms_types.typecode_interval_ym
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as interval year to month));
          when dbms_types.typecode_interval_ds
          then
            dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as interval day to second));
          when dbms_types.typecode_clob
          then
            case pkg_pipeline.r_sql.description(i).col_type
              when 8
              then
                dbms_sql.define_column_long(pkg_pipeline.r_sql.cursor, i);
              else
                dbms_sql.define_column(pkg_pipeline.r_sql.cursor, i, cast(null as clob));
            end case;
        end case;
      end loop;

      -- execute the SQL.
      pkg_pipeline.r_sql.execute := dbms_sql.execute(pkg_pipeline.r_sql.cursor);

      return odciconst.success;

    end ODCITablestart;


    /*
     * FETCH step.
     */
    member function ODCITablefetch(self   in out dyn_pipeline,
                                   nrows  in     number,
                                   rws    out    anydataset)
      return number
    is

      /* Variables */
      -- Buffers to hold values.
      v_vc_col       varchar2(32767);
      v_num_col      number;
      v_date_col     date;
      v_raw_col      raw(32767);
      v_raw_error    number;
      v_raw_len      integer;
      v_int_ds_col   interval day to second;
      v_int_ym_col   interval year to month;
      v_ts_col       timestamp;
      v_tstz_col     timestamp with time zone;
      v_tsltz_col    timestamp with local time zone;
      v_clob_col     clob;
      v_clob_offset  integer := 0;
      v_clob_len     integer;
      -- Metadata
      r_meta  pkg_pipeline.anytype_metadata_rec;

    begin

      if dbms_sql.fetch_rows( pkg_pipeline.r_sql.cursor ) > 0
      then

        -- Describe to get number and types of columns.
        r_meta.typecode := self.atype.getinfo(
                             r_meta.precision, r_meta.scale, r_meta.length,
                             r_meta.csid, r_meta.csfrm, r_meta.schema,
                             r_meta.name, r_meta.version, r_meta.attr_cnt
                           );

        anydataset.begincreate(dbms_types.typecode_object, self.atype, rws);
        rws.addinstance();
        rws.piecewise();

        -- loop through each column extracting value.
        for i in 1..pkg_pipeline.r_sql.column_cnt
        loop

          r_meta.typecode := self.atype.getattreleminfo(
                               i, r_meta.precision, r_meta.scale, r_meta.length,
                               r_meta.csid, r_meta.csfrm, r_meta.attr_type,
                               r_meta.attr_name
                             );

          case r_meta.typecode
            when dbms_types.typecode_varchar2
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_vc_col);
              rws.setvarchar2(v_vc_col);
            when dbms_types.typecode_number
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_num_col);
              rws.setnumber(v_num_col);
            when dbms_types.typecode_date
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_date_col);
              rws.setdate(v_date_col);
            when dbms_types.typecode_raw
            then
              dbms_sql.column_value_raw(pkg_pipeline.r_sql.cursor, i, v_raw_col,
                 v_raw_error, v_raw_len);
              rws.setraw(v_raw_col);
            when dbms_types.typecode_interval_ds
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_int_ds_col);
              rws.setintervalds(v_int_ds_col);
            when dbms_types.typecode_interval_ym
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_int_ym_col);
              rws.setintervalym(v_int_ym_col);
            when dbms_types.typecode_timestamp
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_ts_col);
              rws.settimestamp(v_ts_col);
            when dbms_types.typecode_timestamp_tz
            then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_tstz_col);
              rws.settimestamptz(v_tstz_col);
           when dbms_types.typecode_timestamp_ltz
           then
              dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_tsltz_col);
              rws.settimestampltz(v_tsltz_col);
           when dbms_types.typecode_clob
           then
             case pkg_pipeline.r_sql.description(i).col_type
               when 8
               then
                 loop
                   dbms_sql.column_value_long(pkg_pipeline.r_sql.cursor, i, 32767, v_clob_offset,
                                              v_vc_col, v_clob_len);
                   v_clob_col := v_clob_col || v_vc_col;
                   v_clob_offset := v_clob_offset + 32767;
                   exit when v_clob_len < 32767;
                 end loop;
               else
                 dbms_sql.column_value(pkg_pipeline.r_sql.cursor, i, v_clob_col);
             end case;
             rws.setclob(v_clob_col);
          end case;
        end loop;

        rws.endcreate();

      end if;

      return ODCIConst.Success;

    end;

    /*
     * CLOSE step. close the cursor.
     */
    member function ODCITableClose(self in dyn_pipeline)
      return number
    is


    begin
      dbms_sql.close_cursor( pkg_pipeline.r_sql.cursor );
      pkg_pipeline.r_sql := null;
      return odciconst.success;
    end ODCITableClose;

  end;
  /

  --select * from table(pkg_pipeline.querydb('select * from dual'));

  
CREATE OR REPLACE FUNCTION FIRST_COL_QUERY(stmt VARCHAR2)
RETURN  VARCHAR2 DETERMINISTIC
IS 
v_sql VARCHAR2(32757);
r_sql   pkg_pipeline.dynamic_sql_rec;
BEGIN
v_sql := replace(stmt, ';', null);
r_sql.cursor := dbms_sql.open_cursor;
dbms_sql.parse( r_sql.cursor, v_sql, dbms_sql.native );
dbms_sql.describe_columns2( r_sql.cursor, r_sql.column_cnt, r_sql.description );
dbms_sql.close_cursor( r_sql.cursor );
RETURN r_sql.description(1).col_name;
END;
/
  
CREATE OR REPLACE FUNCTION "GENERATE_VARCHAR_SET_QUERY" (SMT VARCHAR2)
RETURN VARCHAR2 IS
A VARCHAR2(32767);
FIELD_N VARCHAR2(32767):=FIRST_COL_QUERY(SMT);
Q VARCHAR2(32767):='SELECT * FROM (select * from ('||SMT||') ORDER BY dbms_random.value ) WHERE ROWNUM=1'; 
Q2 VARCHAR2(32767):='SELECT COUNT(*) FROM ('||SMT||')'; 
Q3 VARCHAR2(32767):='SELECT '||FIELD_N||' FROM (SELECT '||FIELD_N||',ROWNUM R FROM ('||SMT||')) WHERE R = :N '; 
N NUMBER(10);
I NUMBER(5);
BEGIN
 EXECUTE IMMEDIATE Q2 INTO N; 
 EXECUTE IMMEDIATE Q3 INTO A USING ROUND(DBMS_RANDOM.VALUE(1,N));
-- EXECUTE IMMEDIATE Q INTO A;
 RETURN A;
END GENERATE_VARCHAR_SET_QUERY;
/

/*
SELECT 
TO_NUMBER(GENERATE_VARCHAR_SET_QUERY('SELECT IP_ID FROM DMRPMCOR.IP@DLKDMR')) A,
GENERATE_VARCHAR_SET_QUERY('SELECT IP_NM FROM IP') B,
GENERATE_VARCHAR_SET(VAR_ARRAY(1,2,3)) C
FROM ALL_TABLES WHERE ROWNUM<100;

*/

--------------------------------------------------------
--  DDL for Type CLOBAGG_TYPE
--------------------------------------------------------

  CREATE OR REPLACE TYPE "CLOBAGG_TYPE" as object(
                              text clob,
                              static function ODCIAggregateInitialize(
                                                                      sctx in out clobagg_type
                                                                     )
                                return number,
                              member function ODCIAggregateIterate(
                                                                   self  in out clobagg_type,
                                                                   value in     clob
                                                                  )
                                return number,
                              member function ODCIAggregateTerminate(
                                                                     self        in  clobagg_type,
                                                                     returnvalue out clob,
                                                                     flags in number
                                                                    )
                                return number,
                              member function ODCIAggregateMerge(
                                                                 self in out clobagg_type,
                                                                 ctx2 in     clobagg_type
                                                                )
                                return number
                             );
                             
/

CREATE OR REPLACE TYPE BODY "CLOBAGG_TYPE" 
    is
      static function ODCIAggregateInitialize(
                                              sctx in out clobagg_type
                                             )
        return number
        is
        begin
            sctx := clobagg_type(null) ;
            return ODCIConst.Success ;
      end;
      member function ODCIAggregateIterate(
                                           self  in out clobagg_type,
                                           value in     clob
                                          )
        return number
        is
        begin
            self.text := self.text || value ;
            return ODCIConst.Success;
      end;
      member function ODCIAggregateTerminate(
                                             self        in  clobagg_type,
                                             returnvalue out clob,
                                             flags       in  number
                                            )
        return number
        is
        begin
            returnValue := self.text;
            return ODCIConst.Success;
        end;
      member function ODCIAggregateMerge(
                                         self in out clobagg_type,
                                         ctx2 in     clobagg_type
                                        )
        return number
        is
        begin
            self.text := self.text || ctx2.text;
            return ODCIConst.Success;
        end;
end;
/


--------------------------------------------------------
--  DDL for Type CONCAT_EXPR
--------------------------------------------------------

  CREATE OR REPLACE TYPE "CONCAT_EXPR" AS OBJECT (
 str clob,
 del VARCHAR2 (4000));
 /
--------------------------------------------------------
--  DDL for Type CONCAT_ALL_OT
--------------------------------------------------------

  CREATE OR REPLACE TYPE "CONCAT_ALL_OT" AS OBJECT (
      str CLOB,
      del VARCHAR2 (4000),
    
      STATIC FUNCTION odciaggregateinitialize (
        sctx IN OUT concat_all_ot)
        RETURN NUMBER,
    
      MEMBER FUNCTION odciaggregateiterate (
       SELF IN OUT concat_all_ot,
       ctx IN concat_expr)
       RETURN NUMBER,
   
     MEMBER FUNCTION odciaggregateterminate (
       SELF IN concat_all_ot,
       returnvalue OUT CLOB,
       flags IN NUMBER)
      RETURN NUMBER,
   
     MEMBER FUNCTION odciaggregatemerge (
       SELF IN OUT concat_all_ot,
       ctx2 concat_all_ot)
       RETURN NUMBER);
/

CREATE OR REPLACE TYPE BODY "CONCAT_ALL_OT" 
    AS
     STATIC FUNCTION odciaggregateinitialize (
        sctx IN OUT concat_all_ot)
        RETURN NUMBER
     IS
      BEGIN
        sctx := concat_all_ot (NULL, NULL);
        RETURN odciconst.success;
     END;
   
     MEMBER FUNCTION odciaggregateiterate (
       SELF IN OUT concat_all_ot,
       ctx IN concat_expr)
       RETURN NUMBER
     IS
     BEGIN
       IF ctx.str IS NOT NULL THEN
       IF SELF.str IS NULL THEN
         SELF.str := ctx.str;
       ELSE
         SELF.str := SELF.str || ctx.del || ctx.str;    
       END IF;
       END IF;       
       RETURN odciconst.success;
     END;
   
     MEMBER FUNCTION odciaggregateterminate (
       SELF IN concat_all_ot,
       returnvalue OUT CLOB,
       flags IN NUMBER)
       RETURN NUMBER
     IS
     BEGIN
       returnvalue := SELF.str;
       RETURN odciconst.success;
     END;
   
     MEMBER FUNCTION odciaggregatemerge (
       SELF IN OUT concat_all_ot,
       ctx2 IN concat_all_ot)
       RETURN NUMBER
     IS
     BEGIN
       IF ctx2.str IS NOT NULL THEN
         IF SELF.str IS NULL THEN
          SELF.str := ctx2.str;
          ELSE
         SELF.str := SELF.str || SELF.del || ctx2.str;
         END IF;
       END IF;       
       RETURN odciconst.success;
     END;
   END;
/

--------------------------------------------------------
--  DDL for Function CONCAT_ALL
--------------------------------------------------------

  CREATE OR REPLACE FUNCTION "CONCAT_ALL" (
   ctx IN concat_expr)
     RETURN CLOB DETERMINISTIC PARALLEL_ENABLE
    AGGREGATE USING concat_all_ot;
/

/*

SELECT 
CONCAT_ALL (CONCAT_EXPR (COLUMN_NAME,',')) ABC,
RTRIM(XMLAGG(XMLELEMENT(e,COLUMN_NAME,',').EXTRACT('//text()') ORDER BY COLUMN_ID).GETCLOBVAL(),',') abcd
FROM ALL_TAB_COLUMNS WHERE TABLE_NAME='IP' ORDER BY COLUMN_ID;

*/


CREATE TABLE ANSI 
(	
COD NUMBER, 
A NUMBER
);
/

Insert into ANSI (COD,A) values ('1','32');
Insert into ANSI (COD,A) values ('2','33');
Insert into ANSI (COD,A) values ('3','34');
Insert into ANSI (COD,A) values ('4','35');
Insert into ANSI (COD,A) values ('5','36');
Insert into ANSI (COD,A) values ('6','37');
Insert into ANSI (COD,A) values ('7','38');
Insert into ANSI (COD,A) values ('8','39');
Insert into ANSI (COD,A) values ('9','40');
Insert into ANSI (COD,A) values ('10','41');
Insert into ANSI (COD,A) values ('11','42');
Insert into ANSI (COD,A) values ('12','43');
Insert into ANSI (COD,A) values ('13','44');
Insert into ANSI (COD,A) values ('14','45');
Insert into ANSI (COD,A) values ('15','46');
Insert into ANSI (COD,A) values ('16','47');
Insert into ANSI (COD,A) values ('17','48');
Insert into ANSI (COD,A) values ('18','49');
Insert into ANSI (COD,A) values ('19','50');
Insert into ANSI (COD,A) values ('20','51');
Insert into ANSI (COD,A) values ('21','52');
Insert into ANSI (COD,A) values ('22','53');
Insert into ANSI (COD,A) values ('23','54');
Insert into ANSI (COD,A) values ('24','55');
Insert into ANSI (COD,A) values ('25','56');
Insert into ANSI (COD,A) values ('26','57');
Insert into ANSI (COD,A) values ('27','58');
Insert into ANSI (COD,A) values ('28','59');
Insert into ANSI (COD,A) values ('29','60');
Insert into ANSI (COD,A) values ('30','61');
Insert into ANSI (COD,A) values ('31','62');
Insert into ANSI (COD,A) values ('32','63');
Insert into ANSI (COD,A) values ('33','64');
Insert into ANSI (COD,A) values ('34','65');
Insert into ANSI (COD,A) values ('35','66');
Insert into ANSI (COD,A) values ('36','67');
Insert into ANSI (COD,A) values ('37','68');
Insert into ANSI (COD,A) values ('38','69');
Insert into ANSI (COD,A) values ('39','70');
Insert into ANSI (COD,A) values ('40','71');
Insert into ANSI (COD,A) values ('41','72');
Insert into ANSI (COD,A) values ('42','73');
Insert into ANSI (COD,A) values ('43','74');
Insert into ANSI (COD,A) values ('44','75');
Insert into ANSI (COD,A) values ('45','76');
Insert into ANSI (COD,A) values ('46','77');
Insert into ANSI (COD,A) values ('47','78');
Insert into ANSI (COD,A) values ('48','79');
Insert into ANSI (COD,A) values ('49','80');
Insert into ANSI (COD,A) values ('50','81');
Insert into ANSI (COD,A) values ('51','82');
Insert into ANSI (COD,A) values ('52','83');
Insert into ANSI (COD,A) values ('53','84');
Insert into ANSI (COD,A) values ('54','85');
Insert into ANSI (COD,A) values ('55','86');
Insert into ANSI (COD,A) values ('56','87');
Insert into ANSI (COD,A) values ('57','88');
Insert into ANSI (COD,A) values ('58','89');
Insert into ANSI (COD,A) values ('59','90');
Insert into ANSI (COD,A) values ('60','91');
Insert into ANSI (COD,A) values ('61','92');
Insert into ANSI (COD,A) values ('62','93');
Insert into ANSI (COD,A) values ('63','94');
Insert into ANSI (COD,A) values ('64','95');
Insert into ANSI (COD,A) values ('65','96');
Insert into ANSI (COD,A) values ('66','97');
Insert into ANSI (COD,A) values ('67','98');
Insert into ANSI (COD,A) values ('68','99');
Insert into ANSI (COD,A) values ('69','100');
Insert into ANSI (COD,A) values ('70','101');
Insert into ANSI (COD,A) values ('71','102');
Insert into ANSI (COD,A) values ('72','103');
Insert into ANSI (COD,A) values ('73','104');
Insert into ANSI (COD,A) values ('74','105');
Insert into ANSI (COD,A) values ('75','106');
Insert into ANSI (COD,A) values ('76','107');
Insert into ANSI (COD,A) values ('77','108');
Insert into ANSI (COD,A) values ('78','109');
Insert into ANSI (COD,A) values ('79','110');
Insert into ANSI (COD,A) values ('80','111');
Insert into ANSI (COD,A) values ('81','112');
Insert into ANSI (COD,A) values ('82','113');
Insert into ANSI (COD,A) values ('83','114');
Insert into ANSI (COD,A) values ('84','115');
Insert into ANSI (COD,A) values ('85','116');
Insert into ANSI (COD,A) values ('86','117');
Insert into ANSI (COD,A) values ('87','118');
Insert into ANSI (COD,A) values ('88','119');
Insert into ANSI (COD,A) values ('89','120');
Insert into ANSI (COD,A) values ('90','121');
Insert into ANSI (COD,A) values ('91','122');
Insert into ANSI (COD,A) values ('92','123');
Insert into ANSI (COD,A) values ('93','124');
Insert into ANSI (COD,A) values ('94','125');
Insert into ANSI (COD,A) values ('95','126');
Insert into ANSI (COD,A) values ('96','127');
Insert into ANSI (COD,A) values ('97','128');
Insert into ANSI (COD,A) values ('98','130');
Insert into ANSI (COD,A) values ('99','131');
Insert into ANSI (COD,A) values ('100','132');
Insert into ANSI (COD,A) values ('101','133');
Insert into ANSI (COD,A) values ('102','134');
Insert into ANSI (COD,A) values ('103','135');
Insert into ANSI (COD,A) values ('104','136');
Insert into ANSI (COD,A) values ('105','137');
Insert into ANSI (COD,A) values ('106','138');
Insert into ANSI (COD,A) values ('107','139');
Insert into ANSI (COD,A) values ('108','140');
Insert into ANSI (COD,A) values ('109','142');
Insert into ANSI (COD,A) values ('110','145');
Insert into ANSI (COD,A) values ('111','146');
Insert into ANSI (COD,A) values ('112','147');
Insert into ANSI (COD,A) values ('113','148');
Insert into ANSI (COD,A) values ('114','149');
Insert into ANSI (COD,A) values ('115','150');
Insert into ANSI (COD,A) values ('116','151');
Insert into ANSI (COD,A) values ('117','152');
Insert into ANSI (COD,A) values ('118','153');
Insert into ANSI (COD,A) values ('119','154');
Insert into ANSI (COD,A) values ('120','155');
Insert into ANSI (COD,A) values ('121','156');
Insert into ANSI (COD,A) values ('122','158');
Insert into ANSI (COD,A) values ('123','159');
Insert into ANSI (COD,A) values ('124','160');
Insert into ANSI (COD,A) values ('125','161');
Insert into ANSI (COD,A) values ('126','162');
Insert into ANSI (COD,A) values ('127','163');
Insert into ANSI (COD,A) values ('128','164');
Insert into ANSI (COD,A) values ('129','165');
Insert into ANSI (COD,A) values ('130','166');
Insert into ANSI (COD,A) values ('131','167');
Insert into ANSI (COD,A) values ('132','168');
Insert into ANSI (COD,A) values ('133','169');
Insert into ANSI (COD,A) values ('134','170');
Insert into ANSI (COD,A) values ('135','171');
Insert into ANSI (COD,A) values ('136','172');
Insert into ANSI (COD,A) values ('137','173');
Insert into ANSI (COD,A) values ('138','174');
Insert into ANSI (COD,A) values ('139','175');
Insert into ANSI (COD,A) values ('140','176');
Insert into ANSI (COD,A) values ('141','177');
Insert into ANSI (COD,A) values ('142','178');
Insert into ANSI (COD,A) values ('143','179');
Insert into ANSI (COD,A) values ('144','180');
Insert into ANSI (COD,A) values ('145','181');
Insert into ANSI (COD,A) values ('146','182');
Insert into ANSI (COD,A) values ('147','183');
Insert into ANSI (COD,A) values ('148','184');
Insert into ANSI (COD,A) values ('149','185');
Insert into ANSI (COD,A) values ('150','186');
Insert into ANSI (COD,A) values ('151','187');
Insert into ANSI (COD,A) values ('152','188');
Insert into ANSI (COD,A) values ('153','189');
Insert into ANSI (COD,A) values ('154','190');
Insert into ANSI (COD,A) values ('155','191');
Insert into ANSI (COD,A) values ('156','192');
Insert into ANSI (COD,A) values ('157','193');
Insert into ANSI (COD,A) values ('158','194');
Insert into ANSI (COD,A) values ('159','195');
Insert into ANSI (COD,A) values ('160','196');
Insert into ANSI (COD,A) values ('161','197');
Insert into ANSI (COD,A) values ('162','198');
Insert into ANSI (COD,A) values ('163','199');
Insert into ANSI (COD,A) values ('164','200');
Insert into ANSI (COD,A) values ('165','201');
Insert into ANSI (COD,A) values ('166','202');
Insert into ANSI (COD,A) values ('167','203');
Insert into ANSI (COD,A) values ('168','204');
Insert into ANSI (COD,A) values ('169','205');
Insert into ANSI (COD,A) values ('170','206');
Insert into ANSI (COD,A) values ('171','207');
Insert into ANSI (COD,A) values ('172','208');
Insert into ANSI (COD,A) values ('173','209');
Insert into ANSI (COD,A) values ('174','210');
Insert into ANSI (COD,A) values ('175','211');
Insert into ANSI (COD,A) values ('176','212');
Insert into ANSI (COD,A) values ('177','213');
Insert into ANSI (COD,A) values ('178','214');
Insert into ANSI (COD,A) values ('179','215');
Insert into ANSI (COD,A) values ('180','216');
Insert into ANSI (COD,A) values ('181','217');
Insert into ANSI (COD,A) values ('182','218');
Insert into ANSI (COD,A) values ('183','219');
Insert into ANSI (COD,A) values ('184','220');
Insert into ANSI (COD,A) values ('185','221');
Insert into ANSI (COD,A) values ('186','222');
Insert into ANSI (COD,A) values ('187','223');
Insert into ANSI (COD,A) values ('188','224');
Insert into ANSI (COD,A) values ('189','225');
Insert into ANSI (COD,A) values ('190','226');
Insert into ANSI (COD,A) values ('191','227');
Insert into ANSI (COD,A) values ('192','228');
Insert into ANSI (COD,A) values ('193','229');
Insert into ANSI (COD,A) values ('194','230');
Insert into ANSI (COD,A) values ('195','231');
Insert into ANSI (COD,A) values ('196','232');
Insert into ANSI (COD,A) values ('197','233');
Insert into ANSI (COD,A) values ('198','234');
Insert into ANSI (COD,A) values ('199','235');
Insert into ANSI (COD,A) values ('200','236');
Insert into ANSI (COD,A) values ('201','237');
Insert into ANSI (COD,A) values ('202','238');
Insert into ANSI (COD,A) values ('203','239');
Insert into ANSI (COD,A) values ('204','240');
Insert into ANSI (COD,A) values ('205','241');
Insert into ANSI (COD,A) values ('206','242');
Insert into ANSI (COD,A) values ('207','243');
Insert into ANSI (COD,A) values ('208','244');
Insert into ANSI (COD,A) values ('209','245');
Insert into ANSI (COD,A) values ('210','246');
Insert into ANSI (COD,A) values ('211','247');
Insert into ANSI (COD,A) values ('212','248');
Insert into ANSI (COD,A) values ('213','249');
Insert into ANSI (COD,A) values ('214','250');
Insert into ANSI (COD,A) values ('215','251');
Insert into ANSI (COD,A) values ('216','252');
Insert into ANSI (COD,A) values ('217','253');
Insert into ANSI (COD,A) values ('218','254');
Insert into ANSI (COD,A) values ('219','255');
Insert into ANSI (COD,A) values ('220','256');
/

--------------------------------------------------------
--  DDL for Table CODS
--------------------------------------------------------

CREATE TABLE CODS 
(	
COD NUMBER, 
A NUMBER, 
B VARCHAR2(1)
);
/

REM INSERTING into CODS
SET DEFINE OFF;
Insert into CODS (COD,A,B) values ('1','38','&');
Insert into CODS (COD,A,B) values ('2','48','0');
Insert into CODS (COD,A,B) values ('3','49','1');
Insert into CODS (COD,A,B) values ('4','50','2');
Insert into CODS (COD,A,B) values ('5','51','3');
Insert into CODS (COD,A,B) values ('6','52','4');
Insert into CODS (COD,A,B) values ('7','53','5');
Insert into CODS (COD,A,B) values ('8','54','6');
Insert into CODS (COD,A,B) values ('9','55','7');
Insert into CODS (COD,A,B) values ('10','56','8');
Insert into CODS (COD,A,B) values ('11','57','9');
Insert into CODS (COD,A,B) values ('12','60','<');
Insert into CODS (COD,A,B) values ('13','62','>');
Insert into CODS (COD,A,B) values ('14','65','A');
Insert into CODS (COD,A,B) values ('15','66','B');
Insert into CODS (COD,A,B) values ('16','67','C');
Insert into CODS (COD,A,B) values ('17','68','D');
Insert into CODS (COD,A,B) values ('18','69','E');
Insert into CODS (COD,A,B) values ('19','70','F');
Insert into CODS (COD,A,B) values ('20','71','G');
Insert into CODS (COD,A,B) values ('21','72','H');
Insert into CODS (COD,A,B) values ('22','73','I');
Insert into CODS (COD,A,B) values ('23','74','J');
Insert into CODS (COD,A,B) values ('24','75','K');
Insert into CODS (COD,A,B) values ('25','76','L');
Insert into CODS (COD,A,B) values ('26','77','M');
Insert into CODS (COD,A,B) values ('27','78','N');
Insert into CODS (COD,A,B) values ('28','79','O');
Insert into CODS (COD,A,B) values ('29','80','P');
Insert into CODS (COD,A,B) values ('30','81','Q');
Insert into CODS (COD,A,B) values ('31','82','R');
Insert into CODS (COD,A,B) values ('32','83','S');
Insert into CODS (COD,A,B) values ('33','84','T');
Insert into CODS (COD,A,B) values ('34','85','U');
Insert into CODS (COD,A,B) values ('35','86','V');
Insert into CODS (COD,A,B) values ('36','87','W');
Insert into CODS (COD,A,B) values ('37','88','X');
Insert into CODS (COD,A,B) values ('38','89','Y');
Insert into CODS (COD,A,B) values ('39','90','Z');
Insert into CODS (COD,A,B) values ('40','97','a');
Insert into CODS (COD,A,B) values ('41','98','b');
Insert into CODS (COD,A,B) values ('42','99','c');
Insert into CODS (COD,A,B) values ('43','100','d');
Insert into CODS (COD,A,B) values ('44','101','e');
Insert into CODS (COD,A,B) values ('45','102','f');
Insert into CODS (COD,A,B) values ('46','103','g');
Insert into CODS (COD,A,B) values ('47','104','h');
Insert into CODS (COD,A,B) values ('48','105','i');
Insert into CODS (COD,A,B) values ('49','106','j');
Insert into CODS (COD,A,B) values ('50','107','k');
Insert into CODS (COD,A,B) values ('51','108','l');
Insert into CODS (COD,A,B) values ('52','109','m');
Insert into CODS (COD,A,B) values ('53','110','n');
Insert into CODS (COD,A,B) values ('54','111','o');
Insert into CODS (COD,A,B) values ('55','112','p');
Insert into CODS (COD,A,B) values ('56','113','q');
Insert into CODS (COD,A,B) values ('57','114','r');
Insert into CODS (COD,A,B) values ('58','115','s');
Insert into CODS (COD,A,B) values ('59','116','t');
Insert into CODS (COD,A,B) values ('60','117','u');
Insert into CODS (COD,A,B) values ('61','118','v');
Insert into CODS (COD,A,B) values ('62','119','w');
Insert into CODS (COD,A,B) values ('63','120','x');
Insert into CODS (COD,A,B) values ('64','121','y');
Insert into CODS (COD,A,B) values ('65','122','z');
Insert into CODS (COD,A,B) values ('66','145','‘');
Insert into CODS (COD,A,B) values ('67','147','“');
Insert into CODS (COD,A,B) values ('68','193','Á');
Insert into CODS (COD,A,B) values ('69','201','É');
Insert into CODS (COD,A,B) values ('70','205','Í');
Insert into CODS (COD,A,B) values ('71','209','Ñ');
Insert into CODS (COD,A,B) values ('72','211','Ó');
Insert into CODS (COD,A,B) values ('73','218','Ú');
Insert into CODS (COD,A,B) values ('74','220','Ü');
Insert into CODS (COD,A,B) values ('75','225','á');
Insert into CODS (COD,A,B) values ('76','233','é');
Insert into CODS (COD,A,B) values ('77','237','í');
Insert into CODS (COD,A,B) values ('78','241','ñ');
Insert into CODS (COD,A,B) values ('79','243','ó');
Insert into CODS (COD,A,B) values ('80','250','ú');
Insert into CODS (COD,A,B) values ('81','252','ü');
/
--------------------------------------------------------
--  DDL for Table ISO88591
--------------------------------------------------------

CREATE TABLE ISO88591 
(COD NUMBER, 
A NUMBER
);
/
REM INSERTING into ISO88591
SET DEFINE OFF;
Insert into ISO88591 (COD,A) values ('1','32');
Insert into ISO88591 (COD,A) values ('2','33');
Insert into ISO88591 (COD,A) values ('3','34');
Insert into ISO88591 (COD,A) values ('4','35');
Insert into ISO88591 (COD,A) values ('5','36');
Insert into ISO88591 (COD,A) values ('6','37');
Insert into ISO88591 (COD,A) values ('7','38');
Insert into ISO88591 (COD,A) values ('8','39');
Insert into ISO88591 (COD,A) values ('9','40');
Insert into ISO88591 (COD,A) values ('10','41');
Insert into ISO88591 (COD,A) values ('11','42');
Insert into ISO88591 (COD,A) values ('12','43');
Insert into ISO88591 (COD,A) values ('13','44');
Insert into ISO88591 (COD,A) values ('14','45');
Insert into ISO88591 (COD,A) values ('15','46');
Insert into ISO88591 (COD,A) values ('16','47');
Insert into ISO88591 (COD,A) values ('17','48');
Insert into ISO88591 (COD,A) values ('18','49');
Insert into ISO88591 (COD,A) values ('19','50');
Insert into ISO88591 (COD,A) values ('20','51');
Insert into ISO88591 (COD,A) values ('21','52');
Insert into ISO88591 (COD,A) values ('22','53');
Insert into ISO88591 (COD,A) values ('23','54');
Insert into ISO88591 (COD,A) values ('24','55');
Insert into ISO88591 (COD,A) values ('25','56');
Insert into ISO88591 (COD,A) values ('26','57');
Insert into ISO88591 (COD,A) values ('27','58');
Insert into ISO88591 (COD,A) values ('28','59');
Insert into ISO88591 (COD,A) values ('29','60');
Insert into ISO88591 (COD,A) values ('30','61');
Insert into ISO88591 (COD,A) values ('31','62');
Insert into ISO88591 (COD,A) values ('32','63');
Insert into ISO88591 (COD,A) values ('33','64');
Insert into ISO88591 (COD,A) values ('34','65');
Insert into ISO88591 (COD,A) values ('35','66');
Insert into ISO88591 (COD,A) values ('36','67');
Insert into ISO88591 (COD,A) values ('37','68');
Insert into ISO88591 (COD,A) values ('38','69');
Insert into ISO88591 (COD,A) values ('39','70');
Insert into ISO88591 (COD,A) values ('40','71');
Insert into ISO88591 (COD,A) values ('41','72');
Insert into ISO88591 (COD,A) values ('42','73');
Insert into ISO88591 (COD,A) values ('43','74');
Insert into ISO88591 (COD,A) values ('44','75');
Insert into ISO88591 (COD,A) values ('45','76');
Insert into ISO88591 (COD,A) values ('46','77');
Insert into ISO88591 (COD,A) values ('47','78');
Insert into ISO88591 (COD,A) values ('48','79');
Insert into ISO88591 (COD,A) values ('49','80');
Insert into ISO88591 (COD,A) values ('50','81');
Insert into ISO88591 (COD,A) values ('51','82');
Insert into ISO88591 (COD,A) values ('52','83');
Insert into ISO88591 (COD,A) values ('53','84');
Insert into ISO88591 (COD,A) values ('54','85');
Insert into ISO88591 (COD,A) values ('55','86');
Insert into ISO88591 (COD,A) values ('56','87');
Insert into ISO88591 (COD,A) values ('57','88');
Insert into ISO88591 (COD,A) values ('58','89');
Insert into ISO88591 (COD,A) values ('59','90');
Insert into ISO88591 (COD,A) values ('60','91');
Insert into ISO88591 (COD,A) values ('61','92');
Insert into ISO88591 (COD,A) values ('62','93');
Insert into ISO88591 (COD,A) values ('63','94');
Insert into ISO88591 (COD,A) values ('64','95');
Insert into ISO88591 (COD,A) values ('65','96');
Insert into ISO88591 (COD,A) values ('66','97');
Insert into ISO88591 (COD,A) values ('67','98');
Insert into ISO88591 (COD,A) values ('68','99');
Insert into ISO88591 (COD,A) values ('69','100');
Insert into ISO88591 (COD,A) values ('70','101');
Insert into ISO88591 (COD,A) values ('71','102');
Insert into ISO88591 (COD,A) values ('72','103');
Insert into ISO88591 (COD,A) values ('73','104');
Insert into ISO88591 (COD,A) values ('74','105');
Insert into ISO88591 (COD,A) values ('75','106');
Insert into ISO88591 (COD,A) values ('76','107');
Insert into ISO88591 (COD,A) values ('77','108');
Insert into ISO88591 (COD,A) values ('78','109');
Insert into ISO88591 (COD,A) values ('79','110');
Insert into ISO88591 (COD,A) values ('80','111');
Insert into ISO88591 (COD,A) values ('81','112');
Insert into ISO88591 (COD,A) values ('82','113');
Insert into ISO88591 (COD,A) values ('83','114');
Insert into ISO88591 (COD,A) values ('84','115');
Insert into ISO88591 (COD,A) values ('85','116');
Insert into ISO88591 (COD,A) values ('86','117');
Insert into ISO88591 (COD,A) values ('87','118');
Insert into ISO88591 (COD,A) values ('88','119');
Insert into ISO88591 (COD,A) values ('89','120');
Insert into ISO88591 (COD,A) values ('90','121');
Insert into ISO88591 (COD,A) values ('91','122');
Insert into ISO88591 (COD,A) values ('92','123');
Insert into ISO88591 (COD,A) values ('93','124');
Insert into ISO88591 (COD,A) values ('94','125');
Insert into ISO88591 (COD,A) values ('95','126');
Insert into ISO88591 (COD,A) values ('96','160');
Insert into ISO88591 (COD,A) values ('97','161');
Insert into ISO88591 (COD,A) values ('98','162');
Insert into ISO88591 (COD,A) values ('99','163');
Insert into ISO88591 (COD,A) values ('100','164');
Insert into ISO88591 (COD,A) values ('101','165');
Insert into ISO88591 (COD,A) values ('102','166');
Insert into ISO88591 (COD,A) values ('103','167');
Insert into ISO88591 (COD,A) values ('104','168');
Insert into ISO88591 (COD,A) values ('105','169');
Insert into ISO88591 (COD,A) values ('106','170');
Insert into ISO88591 (COD,A) values ('107','171');
Insert into ISO88591 (COD,A) values ('108','172');
Insert into ISO88591 (COD,A) values ('109','173');
Insert into ISO88591 (COD,A) values ('110','174');
Insert into ISO88591 (COD,A) values ('111','175');
Insert into ISO88591 (COD,A) values ('112','176');
Insert into ISO88591 (COD,A) values ('113','177');
Insert into ISO88591 (COD,A) values ('114','178');
Insert into ISO88591 (COD,A) values ('115','179');
Insert into ISO88591 (COD,A) values ('116','180');
Insert into ISO88591 (COD,A) values ('117','181');
Insert into ISO88591 (COD,A) values ('118','182');
Insert into ISO88591 (COD,A) values ('119','183');
Insert into ISO88591 (COD,A) values ('120','184');
Insert into ISO88591 (COD,A) values ('121','185');
Insert into ISO88591 (COD,A) values ('122','186');
Insert into ISO88591 (COD,A) values ('123','187');
Insert into ISO88591 (COD,A) values ('124','188');
Insert into ISO88591 (COD,A) values ('125','189');
Insert into ISO88591 (COD,A) values ('126','190');
Insert into ISO88591 (COD,A) values ('127','191');
Insert into ISO88591 (COD,A) values ('128','192');
Insert into ISO88591 (COD,A) values ('129','193');
Insert into ISO88591 (COD,A) values ('130','194');
Insert into ISO88591 (COD,A) values ('131','195');
Insert into ISO88591 (COD,A) values ('132','196');
Insert into ISO88591 (COD,A) values ('133','197');
Insert into ISO88591 (COD,A) values ('134','198');
Insert into ISO88591 (COD,A) values ('135','199');
Insert into ISO88591 (COD,A) values ('136','200');
Insert into ISO88591 (COD,A) values ('137','201');
Insert into ISO88591 (COD,A) values ('138','202');
Insert into ISO88591 (COD,A) values ('139','203');
Insert into ISO88591 (COD,A) values ('140','204');
Insert into ISO88591 (COD,A) values ('141','205');
Insert into ISO88591 (COD,A) values ('142','206');
Insert into ISO88591 (COD,A) values ('143','207');
Insert into ISO88591 (COD,A) values ('144','208');
Insert into ISO88591 (COD,A) values ('145','209');
Insert into ISO88591 (COD,A) values ('146','210');
Insert into ISO88591 (COD,A) values ('147','211');
Insert into ISO88591 (COD,A) values ('148','212');
Insert into ISO88591 (COD,A) values ('149','213');
Insert into ISO88591 (COD,A) values ('150','214');
Insert into ISO88591 (COD,A) values ('151','215');
Insert into ISO88591 (COD,A) values ('152','216');
Insert into ISO88591 (COD,A) values ('153','217');
Insert into ISO88591 (COD,A) values ('154','218');
Insert into ISO88591 (COD,A) values ('155','219');
Insert into ISO88591 (COD,A) values ('156','220');
Insert into ISO88591 (COD,A) values ('157','221');
Insert into ISO88591 (COD,A) values ('158','222');
Insert into ISO88591 (COD,A) values ('159','223');
Insert into ISO88591 (COD,A) values ('160','224');
Insert into ISO88591 (COD,A) values ('161','225');
Insert into ISO88591 (COD,A) values ('162','226');
Insert into ISO88591 (COD,A) values ('163','227');
Insert into ISO88591 (COD,A) values ('164','228');
Insert into ISO88591 (COD,A) values ('165','229');
Insert into ISO88591 (COD,A) values ('166','230');
Insert into ISO88591 (COD,A) values ('167','231');
Insert into ISO88591 (COD,A) values ('168','232');
Insert into ISO88591 (COD,A) values ('169','233');
Insert into ISO88591 (COD,A) values ('170','234');
Insert into ISO88591 (COD,A) values ('171','235');
Insert into ISO88591 (COD,A) values ('172','236');
Insert into ISO88591 (COD,A) values ('173','237');
Insert into ISO88591 (COD,A) values ('174','238');
Insert into ISO88591 (COD,A) values ('175','239');
Insert into ISO88591 (COD,A) values ('176','240');
Insert into ISO88591 (COD,A) values ('177','241');
Insert into ISO88591 (COD,A) values ('178','242');
Insert into ISO88591 (COD,A) values ('179','243');
Insert into ISO88591 (COD,A) values ('180','244');
Insert into ISO88591 (COD,A) values ('181','245');
Insert into ISO88591 (COD,A) values ('182','246');
Insert into ISO88591 (COD,A) values ('183','247');
Insert into ISO88591 (COD,A) values ('184','248');
Insert into ISO88591 (COD,A) values ('185','249');
Insert into ISO88591 (COD,A) values ('186','250');
Insert into ISO88591 (COD,A) values ('187','251');
Insert into ISO88591 (COD,A) values ('188','252');
Insert into ISO88591 (COD,A) values ('189','253');
Insert into ISO88591 (COD,A) values ('190','254');
Insert into ISO88591 (COD,A) values ('191','255');
/
--------------------------------------------------------
--  DDL for Table NUMBER_ERRORS
--------------------------------------------------------

CREATE TABLE NUMBER_ERRORS 
(VAL VARCHAR2(255)
);
/
-----------
REM INSERTING into NUMBER_ERRORS
SET DEFINE OFF;
REM INSERTING into NUMBER_ERRORS
SET DEFINE OFF;
Insert into NUMBER_ERRORS (VAL) values (' ');
Insert into NUMBER_ERRORS (VAL) values ('1,1');
Insert into NUMBER_ERRORS (VAL) values ('1.1');
Insert into NUMBER_ERRORS (VAL) values ('+1');
Insert into NUMBER_ERRORS (VAL) values ('-1');
Insert into NUMBER_ERRORS (VAL) values ('+');
Insert into NUMBER_ERRORS (VAL) values ('-');
Insert into NUMBER_ERRORS (VAL) values ('A');
Insert into NUMBER_ERRORS (VAL) values ('1');
Insert into NUMBER_ERRORS (VAL) values ('+-1');
Insert into NUMBER_ERRORS (VAL) values ('AE');
Insert into NUMBER_ERRORS (VAL) values ('1AE');
Insert into NUMBER_ERRORS (VAL) values ('AE1');
Insert into NUMBER_ERRORS (VAL) values ('-0000000000000001,00');
Insert into NUMBER_ERRORS (VAL) values ('-0000000000000000001,00');
Insert into NUMBER_ERRORS (VAL) values ('-0000000000000001.00');
Insert into NUMBER_ERRORS (VAL) values ('-0000000000000000001.00');
Insert into NUMBER_ERRORS (VAL) values ('+0000000000000001,00');
Insert into NUMBER_ERRORS (VAL) values ('+0000000000000000001,00');
Insert into NUMBER_ERRORS (VAL) values ('+0000000000000001.00');
Insert into NUMBER_ERRORS (VAL) values ('+0000000000000000001.00');
Insert into NUMBER_ERRORS (VAL) values ('+000000000000000001.000');
Insert into NUMBER_ERRORS (VAL) values ('000000000000000000.1000');
/

--------------------------------------------------------
--  DDL for Function NULLS_F
--------------------------------------------------------

CREATE OR REPLACE FUNCTION NULLS_F 
(
p_string VARCHAR2,
p_del VARCHAR2 DEFAULT ','
) RETURN varchar2
IS
BEGIN
RETURN REGEXP_REPLACE(REGEXP_REPLACE(p_string,'['||p_del||']['||p_del||']',p_del||'<NULL>'||p_del),'['||p_del||']['||p_del||']',p_del||'<NULL>'||p_del);
END NULLS_F;
/
--------------------------------------------------------
--  DDL for Function FIELD
--------------------------------------------------------

CREATE OR REPLACE FUNCTION FIELD 
(
p_string VARCHAR2,
p_del VARCHAR2 DEFAULT ',',
p_ocurrence NUMBER
) RETURN varchar2
IS
BEGIN
RETURN REPLACE(REGEXP_SUBSTR(REPLACE(NULLS_F(p_string,p_del),p_del,CHR(141)), '[^'||CHR(141)||']+', 1, p_ocurrence),'<NULL>');
END FIELD;
/

--------------------------------------------------------
--  DDL for Function GC
--------------------------------------------------------

CREATE OR REPLACE FUNCTION GC 
RETURN VARCHAR
AS
C VARCHAR(1);
BEGIN
SELECT B INTO C
FROM
CODS
WHERE 
COD=(SELECT CEIL(DBMS_RANDOM.VALUE(0,(SELECT COUNT(*) FROM CODS))) FROM DUAL);
RETURN C;
END GC;
/

--------------------------------------------------------
--  DDL for Function GENERATE_CHAR
--------------------------------------------------------

CREATE OR REPLACE FUNCTION GENERATE_CHAR
  RETURN VARCHAR2
IS
  S CHAR(1);
BEGIN
 SELECT CHR(A) INTO S FROM ISO88591 WHERE COD=(SELECT ROUND(DBMS_RANDOM.VALUE(1,191)) A FROM DUAL);
  RETURN S;
EXCEPTION
  WHEN OTHERS THEN
    RETURN NULL;
END GENERATE_CHAR;
/

CREATE OR REPLACE FUNCTION SIGNO
RETURN NUMBER
AS
BEGIN
RETURN CASE WHEN DBMS_RANDOM.VALUE >0.5 THEN 1 ELSE -1 END;
END;
/



-------------------------------------------------------
--  DDL for Function GENERATE_NUMBER
--------------------------------------------------------
CREATE OR REPLACE FUNCTION GENERATE_NUMBER (N IN NUMBER, D IN NUMBER DEFAULT NULL,S IN VARCHAR2 DEFAULT NULL)
RETURN NUMBER
IS
  R NUMBER;
BEGIN
  SELECT CASE WHEN S='S' THEN SIGNO ELSE 1 END*ROUND(DBMS_RANDOM.VALUE(1,POWER(10,N)-1),NVL(D,0)) INTO R FROM DUAL;
  RETURN R;
EXCEPTION
  WHEN OTHERS THEN
    RETURN NULL;
END GENERATE_NUMBER;
/

-------------------------------------------------------
--  DDL for Function PROB
--------------------------------------------------------
CREATE OR REPLACE FUNCTION PROB(STR VARCHAR2,P NUMBER)
RETURN VARCHAR2
IS
BEGIN
RETURN CASE WHEN dbms_random.VALUE <= P THEN STR ELSE NULL END;
END;
/

CREATE OR REPLACE TYPE T_TEXT IS TABLE OF CLOB;
/

--------------------------------------------------------
--  DDL for Function MULTIPLE_REPLACE
--------------------------------------------------------

  CREATE OR REPLACE FUNCTION "MULTIPLE_REPLACE" (
  in_text IN CLOB, in_old IN t_text, in_new IN t_text
)
  RETURN CLOB
AS
  v_result  CLOB;
BEGIN
  IF( in_old.COUNT <> in_new.COUNT ) THEN
    RETURN in_text;
  END IF;
  v_result := in_text;
  FOR i IN 1 .. in_old.COUNT LOOP
    v_result := REPLACE( v_result, in_old(i), in_new(i) );
  END LOOP;
  RETURN v_result;
END;
/
/*
SELECT 
MULTIPLE_REPLACE('ABC',NEW T_TEXT('A','B'),NEW T_TEXT('1','2'))
FROM DUAL;
*/


CREATE OR REPLACE FUNCTION ADD_PLUS(S VARCHAR2)
RETURN VARCHAR2
IS 
BEGIN
RETURN CASE WHEN SUBSTR(S,0,1)='-' THEN S ELSE '+'||S END;
END;
/

CREATE OR REPLACE FUNCTION GENERATE_DECIMAL (M VARCHAR2,N IN NUMBER,D IN NUMBER DEFAULT NULL,SIG IN VARCHAR2 DEFAULT NULL,SEP IN VARCHAR2)
RETURN VARCHAR2
IS
  
BEGIN  
  RETURN MULTIPLE_REPLACE(M,NEW T_TEXT('<INT>','<SEP>','<DEC>'),NEW T_TEXT(TO_CHAR(GENERATE_NUMBER(N,0,SIG)),SEP,LPAD(TO_CHAR(GENERATE_NUMBER(D)),D,'0')));
EXCEPTION
  WHEN OTHERS THEN
    RETURN NULL;
END GENERATE_DECIMAL;
/

/*
SELECT ADD_PLUS(GENERATE_DECIMAL('<INT><SEP><DEC>','15','2','S','.')) FROM ALL_TABLES;
*/


CREATE OR REPLACE FUNCTION GENERATE_DATE_VARCHAR(TOL IN NUMBER,M IN VARCHAR2,D IN DATE DEFAULT SYSDATE)
RETURN VARCHAR IS
BEGIN
RETURN TO_CHAR(D+SIGNO*DBMS_RANDOM.VALUE(0,TOL),M);
END;
/
/*
SELECT 
GENERATE_DATE_VARCHAR(20,'DD/MM/YYYY HH24:MI:SS',TO_DATE('20100101','YYYYMMDD'))
FROM ALL_TABLES;
*/


create or replace FUNCTION "FIELD2" 
(
p_string VARCHAR2,
p_del VARCHAR2 DEFAULT ',',
p_ocurrence NUMBER
) RETURN varchar2
IS
BEGIN
RETURN FIELD(chr(157)||REPLACE(p_string,p_del,chr(157)),CHR(157),p_ocurrence);
END FIELD2;
/


DROP FUNCTION GENERATE_SRC_DEST;
CREATE OR REPLACE FUNCTION "GENERATE_SRC_DEST" (HINTS CLOB,SELECTIONS CLOB,SRC CLOB,DEST CLOB,JOINS CLOB) RETURN CLOB
AS 
BEGIN
RETURN MULTIPLE_REPLACE(
q'#SELECT 
/*+
<HINTS>
*/
<SELECTIONS>
FROM
(
<SRC>
)FUENTE
LEFT JOIN
(
<DEST>
)DESTINO
ON
<JOINS>
ORDER BY 2 DESC;
#'
,NEW T_TEXT('<HINTS>','<SELECTIONS>','<SRC>','<DEST>','<JOINS>')
,NEW T_TEXT(HINTS,SELECTIONS,SRC,DEST,JOINS)
);
END;
/

DROP FUNCTION "ENCLOSE_A";
CREATE OR REPLACE FUNCTION "ENCLOSE_A" (QUERY_ANT CLOB,FIELDS VARCHAR2,HINTS VARCHAR2,JOINS VARCHAR2) RETURN CLOB
IS
BEGIN
RETURN
MULTIPLE_REPLACE(
'
SELECT
<HINTS>
<FIELDS>
FROM
(
<QUERY_ANT>
)A <JOINS>',
t_text( '<FIELDS>', '<QUERY_ANT>','<HINTS>','<JOINS>'),
NEW t_text( FIELDS, QUERY_ANT,CASE WHEN HINTS IS NOT NULL THEN '/*+'||CHR(10)||HINTS||CHR(10)||'*/' END,CASE WHEN JOINS IS NOT NULL THEN CHR(10)||JOINS END));
END ENCLOSE_A;
/

DROP FUNCTION "MULTIPLE_ENCLOSE_A";
CREATE OR REPLACE FUNCTION "MULTIPLE_ENCLOSE_A" (
  IN_TEXT IN CLOB, IN_FIELDS IN T_TEXT, IN_HINTS IN T_TEXT,IN_JOINS IN T_TEXT
)
  RETURN CLOB
AS
  V_RESULT CLOB;
BEGIN
  IF( IN_FIELDS.COUNT <> IN_HINTS.COUNT OR IN_JOINS.COUNT <> IN_HINTS.COUNT) THEN
    RETURN in_text;
  END IF;
  V_RESULT := IN_TEXT;
  FOR I IN 1 .. IN_FIELDS.COUNT LOOP
    V_RESULT := ENCLOSE_A( V_RESULT, IN_FIELDS(I),IN_HINTS(I),IN_JOINS(I));
  END LOOP;
  RETURN V_RESULT;
END;
/

DROP FUNCTION "GENERATE_MULTI_COLUMN";
CREATE OR REPLACE FUNCTION "GENERATE_MULTI_COLUMN" (STR CLOB,SEP VARCHAR2,SEP_STR VARCHAR2,STR_PTR VARCHAR2)
RETURN
CLOB
AS
RES CLOB;
BEGIN
SELECT 
CONCAT_ALL (CONCAT_EXPR (A,SEP_STR)) A INTO RES
FROM
(
SELECT
MULTIPLE_REPLACE
(
STR_PTR,
NEW T_TEXT('<COLUMN_NAME>','<ROWNUM>'),
NEW T_TEXT(COLUMN_NAME,ROWNUM)
) A
FROM
(
SELECT EXTRACT (VALUE (D), '//ROW/text()').GETSTRINGVAL () AS COLUMN_NAME
FROM (SELECT XMLTYPE (   '<ROWS><ROW>'
                        || REPLACE (STR, SEP, '</ROW><ROW>')
                        || '</ROW></ROWS>'
                       ) AS XMLVAL
FROM DUAL) X,
TABLE (XMLSEQUENCE (EXTRACT (X.XMLVAL, '/ROWS/ROW'))) D
));
RETURN RES;
END
;
/
/*
SELECT GENERATE_MULTI_COLUMN (
'A
B
C',CHR(10),',','<COLUMN_NAME> ANT_<ROWNUM>') FROM DUAL
*/

DROP FUNCTION "GENERATE_ERR";
CREATE OR REPLACE FUNCTION "GENERATE_ERR" (P1 CLOB,P2 CLOB,P3 CLOB,OPC_1 CLOB DEFAULT NULL,OPC_2 CLOB DEFAULT NULL) RETURN CLOB
AS 
BEGIN
RETURN MULTIPLE_REPLACE(
q'#
/*FUENTE ERRORES*/ 
CREATE OR REPLACE VIEW <P1>_FTE_ERR AS 
SELECT 
<OPC_2>
FROM
(
SELECT 
<OPC_1>
ID,CAMPO,DSC,
MAX(CASE WHEN IND='ANT' THEN VALOR END) VALOR_ANT,
MAX(CASE WHEN IND='ACT' THEN VALOR END) VALOR_ACT

FROM 
( 
SELECT  
<OPC_1>
DECODE(IND,'TCA','ACT',IND) IND,ID,CAMPO,VALOR,DSC

FROM 
( 
SELECT 
<OPC_1>
ID, 
SUBSTR(COL,0,3) IND,
SUBSTR(COL,5) CAMPO, 
VAL VALOR, 
DECODE 
(COL,
<P2>
) DSC 
FROM 
(( 
SELECT 
A.* 
FROM 
<P1>_FTE A
) UNPIVOT INCLUDE NULLS(VAL FOR COL IN ( 
<P3>
)) 
) 
)PIV WHERE PIV.DSC<>0 
) WHERE CAMPO IS NOT NULL 
GROUP BY <OPC_1>ID,CAMPO,DSC
)A
;#'
,NEW T_TEXT('<P1>','<P2>','<P3>','<OPC_1>','<OPC_2>')
,NEW T_TEXT(P1,P2,P3,OPC_1,OPC_2)
);
END;

/

CREATE TABLE ALEAT512 AS SELECT ROWNUM A FROM ALL_TABLES WHERE ROWNUM<=512;
/

CREATE OR REPLACE FUNCTION GEN_VARCHAR_ALT2 (len NUMBER, FLG VARCHAR2 DEFAULT 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
        RETURN VARCHAR2 PARALLEL_ENABLE is      -- string of <len> characters
        
        rng  NUMBER;
        n    BINARY_INTEGER;
        ccs  VARCHAR2 (128);    -- candidate character subset
        xstr VARCHAR2 (4000) := NULL;
    BEGIN
    ccs := FLG;
    rng := LENGTH(FLG);

        FOR i IN 1 .. least(len,4000) LOOP
            /* Get random integer within specified range */
            n := TRUNC(rng * dbms_random.value) + 1;
            /* Append character to string  */
            xstr := xstr || SUBSTR(ccs,n,1);
        END LOOP;
        RETURN xstr;
    END GEN_VARCHAR_ALT2;
/


CREATE OR REPLACE FUNCTION GEN_VARCHAR_ALT3 (len NUMBER, FLG VARCHAR2 DEFAULT 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
        RETURN VARCHAR2 PARALLEL_ENABLE is      -- string of <len> characters
        
        rng  NUMBER;
        n    BINARY_INTEGER;
        ccs  VARCHAR2 (256);    -- candidate character subset
        xstr VARCHAR2 (4000) := NULL;
    BEGIN
    ccs := FLG;
    rng := LENGTH(FLG);

        FOR i IN 1 .. least(len,4000) LOOP
            /* Get random integer within specified range */
            n := TRUNC(rng * dbms_random.value) + 1;
            /* Append character to string  */
            xstr := xstr || SUBSTR(ccs,n,1);
        END LOOP;
        RETURN xstr;
    END GEN_VARCHAR_ALT3;
/



CREATE OR REPLACE FUNCTION GEN_VARCHAR_SPA(N NUMBER)
RETURN VARCHAR2
AS
BEGIN
RETURN GEN_VARCHAR_ALT2(N,'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ÁáÉéíÍóÓÚúÑñüÜ');
END;
/


CREATE OR REPLACE FUNCTION GEN_VARCHAR_TOT(N NUMBER)
RETURN VARCHAR2
AS
B VARCHAR2(256);
BEGIN
RETURN GEN_VARCHAR_ALT3(N,CHR(1)||CHR(2)||CHR(3)||CHR(4)||CHR(5)||CHR(6)||CHR(7)||CHR(8)||CHR(9)||CHR(10)||CHR(11)||CHR(12)||CHR(13)||CHR(14)||CHR(15)||CHR(16)||CHR(17)||CHR(18)||CHR(19)||CHR(20)||CHR(21)||CHR(22)||CHR(23)||CHR(24)||CHR(25)||CHR(26)||CHR(27)||CHR(28)||CHR(29)||CHR(30)||CHR(31)||CHR(32)||CHR(33)||CHR(34)||CHR(35)||CHR(36)||CHR(37)||CHR(38)||CHR(39)||CHR(40)||CHR(41)||CHR(42)||CHR(43)||CHR(44)||CHR(45)||CHR(46)||CHR(47)||CHR(48)||CHR(49)||CHR(50)||CHR(51)||CHR(52)||CHR(53)||CHR(54)||CHR(55)||CHR(56)||CHR(57)||CHR(58)||CHR(59)||CHR(60)||CHR(61)||CHR(62)||CHR(63)||CHR(64)||CHR(65)||CHR(66)||CHR(67)||CHR(68)||CHR(69)||CHR(70)||CHR(71)||CHR(72)||CHR(73)||CHR(74)||CHR(75)||CHR(76)||CHR(77)||CHR(78)||CHR(79)||CHR(80)||CHR(81)||CHR(82)||CHR(83)||CHR(84)||CHR(85)||CHR(86)||CHR(87)||CHR(88)||CHR(89)||CHR(90)||CHR(91)||CHR(92)||CHR(93)||CHR(94)||CHR(95)||CHR(96)||CHR(97)||CHR(98)||CHR(99)||CHR(100)||CHR(101)||CHR(102)||CHR(103)||CHR(104)||CHR(105)||CHR(106)||CHR(107)||CHR(108)||CHR(109)||CHR(110)||CHR(111)||CHR(112)||CHR(113)||CHR(114)||CHR(115)||CHR(116)||CHR(117)||CHR(118)||CHR(119)||CHR(120)||CHR(121)||CHR(122)||CHR(123)||CHR(124)||CHR(125)||CHR(126)||CHR(127)||CHR(128)||CHR(129)||CHR(130)||CHR(131)||CHR(132)||CHR(133)||CHR(134)||CHR(135)||CHR(136)||CHR(137)||CHR(138)||CHR(139)||CHR(140)||CHR(141)||CHR(142)||CHR(143)||CHR(144)||CHR(145)||CHR(146)||CHR(147)||CHR(148)||CHR(149)||CHR(150)||CHR(151)||CHR(152)||CHR(153)||CHR(154)||CHR(155)||CHR(156)||CHR(157)||CHR(158)||CHR(159)||CHR(160)||CHR(161)||CHR(162)||CHR(163)||CHR(164)||CHR(165)||CHR(166)||CHR(167)||CHR(168)||CHR(169)||CHR(170)||CHR(171)||CHR(172)||CHR(173)||CHR(174)||CHR(175)||CHR(176)||CHR(177)||CHR(178)||CHR(179)||CHR(180)||CHR(181)||CHR(182)||CHR(183)||CHR(184)||CHR(185)||CHR(186)||CHR(187)||CHR(188)||CHR(189)||CHR(190)||CHR(191)||CHR(192)||CHR(193)||CHR(194)||CHR(195)||CHR(196)||CHR(197)||CHR(198)||CHR(199)||CHR(200)||CHR(201)||CHR(202)||CHR(203)||CHR(204)||CHR(205)||CHR(206)||CHR(207)||CHR(208)||CHR(209)||CHR(210)||CHR(211)||CHR(212)||CHR(213)||CHR(214)||CHR(215)||CHR(216)||CHR(217)||CHR(218)||CHR(219)||CHR(220)||CHR(221)||CHR(222)||CHR(223)||CHR(224)||CHR(225)||CHR(226)||CHR(227)||CHR(228)||CHR(229)||CHR(230)||CHR(231)||CHR(232)||CHR(233)||CHR(234)||CHR(235)||CHR(236)||CHR(237)||CHR(238)||CHR(239)||CHR(240)||CHR(241)||CHR(242)||CHR(243)||CHR(244)||CHR(245)||CHR(246)||CHR(247)||CHR(248)||CHR(249)||CHR(250)||CHR(251)||CHR(252)||CHR(253)||CHR(254)||CHR(255)||CHR(256));
END;
/

CREATE OR REPLACE FUNCTION GEN_VARCHAR_ISO88591(N NUMBER)
RETURN VARCHAR2
AS
BEGIN
RETURN GEN_VARCHAR_ALT3(N, q'ÿ!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ'||'ÿ');
END;
/
