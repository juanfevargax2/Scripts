#$LEASING_OWNER#.SBINCLSDWF
INSERT INTO SBDDES.SBINCLSDWF(ICSNUMIDE,ICSTIPPER,ICSTIPIDE,ICSRAZSOC,ICSPRINOM,ICSSEGNOM,ICSPRIAPE,ICSSEGAPE,ICSTIPSOC,ICSFEACDA,ICSENCTFI,ICSCLACOM,ICSCLILIQ,ICSCLI550,ICSCIIU,ICSSECECO,ICSOFIBAN,ICSAGERET,ICSDOCEXT,ICSLLASAP,ICSROLNEG,ICSESTVIN,ICSTIPVIN) VALUES (ORCHESTRATE.ICSNUMIDE,ORCHESTRATE.ICSTIPPER,ORCHESTRATE.ICSTIPIDE,ORCHESTRATE.ICSRAZSOC,ORCHESTRATE.ICSPRINOM,ORCHESTRATE.ICSSEGNOM,ORCHESTRATE.ICSPRIAPE,ORCHESTRATE.ICSSEGAPE,ORCHESTRATE.ICSTIPSOC,ORCHESTRATE.ICSFEACDA,ORCHESTRATE.ICSENCTFI,ORCHESTRATE.ICSCLACOM,ORCHESTRATE.ICSCLILIQ,ORCHESTRATE.ICSCLI550,ORCHESTRATE.ICSCIIU,ORCHESTRATE.ICSSECECO,ORCHESTRATE.ICSOFIBAN,ORCHESTRATE.ICSAGERET,ORCHESTRATE.ICSDOCEXT,ORCHESTRATE.ICSLLASAP,ORCHESTRATE.ICSROLNEG,ORCHESTRATE.ICSESTVIN,ORCHESTRATE.ICSTIPVIN)
SELECT SC4CTANAL2,SUBSTR(SC4CTANAL2,LENGTH(SC4CTANAL2)-10+1,length(SC4CTANAL2)) from SBDDES.SBMAECLBF4 FETCH FIRST 20 ROWS ONLY
DD/MM/YYYY HH12:MI:SSXFF AM TZR
DD/MM/YYYY HH12:MI:SSXFF AM

select upper(t.table_cat) as Catalog,upper(t.table_schem) as Schem,upper(t.table_name) as t,t.table_text as tableDesc,c.system_column_name as colname_short, 
c.column_name as colname_long,c.column_text as coldesc,c.Type_Name as type,c.column_Size as column_size from sysibm.SQLColumns@DWHLEAS c
inner join sysibm.sqltables@DWHLEAS t on c.table_schem = t.table_schem and c.table_name = t.table_name where c.table_name = 'SBINCLSDWF' and c.table_schem = 'SBDDES'
order by ordinal_position;

select * from dba_ts_quotas;
SELECT * FROM DBA_TABLESPACES;

select df.tablespace_name "Tablespace",totalusedspace "Used MB",(df.totalspace - tu.totalusedspace) "Free MB",df.totalspace "Total MB",
round(100 * ( (df.totalspace - tu.totalusedspace)/ df.totalspace)) "Pct. Free" from
(select tablespace_name,round(sum(bytes) / 1048576) TotalSpace from dba_data_files group by tablespace_name) df,
(select round(sum(bytes)/(1024*1024)) totalusedspace, tablespace_name from dba_segments group by tablespace_name) tu where df.tablespace_name = tu.tablespace_name ;

SELECT REPLACE(REPLACE(q'#SELECT '<T>' T,'<O>' O,COUNT(*) C FROM <O>.<T> UNION ALL#','<T>',TABLE_NAME),'<O>',OWNER) B FROM ALL_TABLES WHERE OWNER='HANPMSAT';
SELECT * FROM ALL_ind_columns;
ALTER INDEX DMRPMCOR.IP__IDX_1 REBUILD;

select b.tablespace_name, tbs_size SizeMb, a.free_space FreeMb from  (select tablespace_name, round(sum(bytes)/1024/1024 ,2) as free_space
from dba_free_space group by tablespace_name) a,(select tablespace_name, sum(bytes)/1024/1024 as tbs_size from dba_data_files
group by tablespace_name) b where a.tablespace_name(+)=b.tablespace_name;

SELECT * FROM NLS_DATABASE_PARAMETERS;
ALTER SESSION SET CURRENT_SCHEMA=DMRFN;
select value from v$nls_parameters where parameter = 'NLS_NUMERIC_CHARACTERS';
alter system set "pga_aggregate_limit"='4G' scope=both sid='*';
alter system set "pga_aggregate_target"='2G' scope=both sid='*';


select  table_name y,
        0 x,
        'CREATE TABLE DMRPMCOR.' ||
        rtrim(table_name) ||
        '('
from    DMRPMCOR.ALL_TABLES
where   owner = upper('DMRPMCOR')
union
select  tc.table_name y,
        column_id x,
        REPLACE(decode(column_id,1,'    ','   ,')||
        rtrim(column_name)|| CHR(9) || CHR(9) ||
        rtrim(data_type) ||
        rtrim(decode(data_type,'DATE',null,'LONG',null,
               'NUMBER',decode(to_char(data_precision),null,null,'('),
               '(')) ||
        rtrim(decode(data_type,
               'DATE',null,
               'CHAR',data_length,
               'VARCHAR2',data_length,
               'NVARCHAR2',data_length,
               'CHAR',data_length,
               'NUMBER',decode(to_char(data_precision),null,null,
                 to_char(data_precision) || ',' || to_char(data_scale)),
               'LONG',null,
               '******ERROR')) ||
        rtrim(decode(data_type,'DATE',null,'LONG',null,
               'NUMBER',decode(to_char(data_precision),null,null,')'),
               ')')) || CHR(9) || CHR(9) ||
        CASE WHEN  tc.data_default is not null then ' DEFAULT '||tc.data_default end || chr(9)||
        rtrim(decode(nullable,'N','NOT NULL',null)),'(******ERROR)')
        
from    T_ALL_TAB_COLUMNS tc
        
where   
     tc.owner = upper('DMRPMCOR')
union
select  table_name y,
        999999 x,
        ');'  || CHR(10)
      /*  ||'  STORAGE('                                   || CHR(10)
        ||'  INITIAL '    || initial_extent              || CHR(10)
        ||'  NEXT '       || next_extent                 || CHR(10)
        ||'  MINEXTENTS ' || min_extents                 || CHR(10)
        ||'  MAXEXTENTS ' || max_extents                 || CHR(10)
        ||'  PCTINCREASE '|| pct_increase                || ')' ||CHR(10)
        ||'  INITRANS '   || ini_trans                   || CHR(10)
        ||'  MAXTRANS '   || max_trans                   || CHR(10)
        ||'  PCTFREE '    || pct_free                    || CHR(10)
        ||'  PCTUSED '    || pct_used                    || CHR(10)
        ||'  PARALLEL (DEGREE ' || rtrim(DEGREE) || ') ' || CHR(10)
        ||'  TABLESPACE ' || rtrim(tablespace_name)      ||CHR(10)*/
        ||'/'||CHR(10)||CHR(10)
from    DMRPMCOR.ALL_TABLES
where   owner = upper('DMRPMCOR')
order by 1,2;

SELECT 
REPLACE(REPLACE(REPLACE(REPLACE(REPLACE('CREATE <U> INDEX <O>.<I> ON <O>.<T> (<I_COL>);'
,'<O>',ALL_IND_COLUMNS.INDEX_OWNER)
,'<I>',ALL_IND_COLUMNS.INDEX_NAME)
,'<T>',ALL_IND_COLUMNS.TABLE_NAME)
,'<U>',REPLACE(ALL_INDEXES.UNIQUENESS,'NONUNIQUE'))

,'<I_COL>',LISTAGG( ALL_IND_COLUMNS.COLUMN_NAME||' '||ALL_IND_COLUMNS.DESCEND, ',') WITHIN GROUP (ORDER BY ALL_IND_COLUMNS.COLUMN_POSITION)) A



FROM
DMRPMCOR.ALL_IND_COLUMNS
INNER JOIN
DMRPMCOR.ALL_INDEXES ON
ALL_IND_COLUMNS.INDEX_NAME=ALL_INDEXES.INDEX_NAME
GROUP BY ALL_IND_COLUMNS.INDEX_OWNER
,ALL_IND_COLUMNS.INDEX_NAME
,ALL_IND_COLUMNS.TABLE_NAME
,ALL_INDEXES.UNIQUENESS
;


SELECT 
REPLACE(REPLACE(REPLACE(REPLACE(REPLACE('ALTER TABLE <O>.<T> ADD CONSTRAINT <C> PRIMARY KEY ( <COLS> ) <US>;'
,'<T>',ALL_CONSTRAINTS.TABLE_NAME)
,'<US>',CASE WHEN ALL_CONSTRAINTS.INDEX_NAME IS NOT NULL THEN 'USING INDEX <O>.'||ALL_CONSTRAINTS.INDEX_NAME END)
,'<O>',ALL_CONSTRAINTS.OWNER)
,'<C>',ALL_CONSTRAINTS.CONSTRAINT_NAME),'<COLS>',LISTAGG(ALL_CONS_COLUMNS.COLUMN_NAME, ',') WITHIN GROUP (ORDER BY 1))

FROM
DMRPMCOR.ALL_CONSTRAINTS
INNER JOIN 
DMRPMCOR.ALL_CONS_COLUMNS ON
ALL_CONS_COLUMNS.TABLE_NAME=ALL_CONSTRAINTS.TABLE_NAME AND
ALL_CONS_COLUMNS.OWNER=ALL_CONSTRAINTS.OWNER AND
ALL_CONS_COLUMNS.CONSTRAINT_NAME=ALL_CONSTRAINTS.CONSTRAINT_NAME
WHERE CONSTRAINT_TYPE='P'
GROUP BY
ALL_CONSTRAINTS.TABLE_NAME,ALL_CONSTRAINTS.OWNER,ALL_CONSTRAINTS.CONSTRAINT_NAME,ALL_CONSTRAINTS.INDEX_NAME;







SELECT 
REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE('ALTER TABLE <O>.<T> ADD CONSTRAINT <C> FOREIGN KEY ( <COLS> ) REFERENCES <O>.<T_R> ( <COLS_R> );'
,'<T>',ALL_CONSTRAINTS.TABLE_NAME)
,'<O>',ALL_CONSTRAINTS.OWNER)
,'<C>',ALL_CONSTRAINTS.CONSTRAINT_NAME)
,'<COLS>',LISTAGG(ALL_CONS_COLUMNS.COLUMN_NAME, ',') WITHIN GROUP (ORDER BY 1))
,'<T_R>',PKS.TABLE_NAME)
,'<COLS_R>',PKS.PKS)


FROM
DMRPMCOR.ALL_CONSTRAINTS
INNER JOIN 
DMRPMCOR.ALL_CONS_COLUMNS ON
ALL_CONS_COLUMNS.TABLE_NAME=ALL_CONSTRAINTS.TABLE_NAME AND
ALL_CONS_COLUMNS.OWNER=ALL_CONSTRAINTS.OWNER AND
ALL_CONS_COLUMNS.CONSTRAINT_NAME=ALL_CONSTRAINTS.CONSTRAINT_NAME
LEFT JOIN
(
SELECT 
ALL_CONSTRAINTS.TABLE_NAME,
ALL_CONSTRAINTS.CONSTRAINT_NAME,
LISTAGG(ALL_CONS_COLUMNS.COLUMN_NAME, ',') WITHIN GROUP (ORDER BY 1) PKS
FROM
DMRPMCOR.ALL_CONSTRAINTS
INNER JOIN 
DMRPMCOR.ALL_CONS_COLUMNS ON
ALL_CONS_COLUMNS.TABLE_NAME=ALL_CONSTRAINTS.TABLE_NAME AND
ALL_CONS_COLUMNS.OWNER=ALL_CONSTRAINTS.OWNER AND
ALL_CONS_COLUMNS.CONSTRAINT_NAME=ALL_CONSTRAINTS.CONSTRAINT_NAME
WHERE CONSTRAINT_TYPE='P'
GROUP BY
ALL_CONSTRAINTS.TABLE_NAME,ALL_CONSTRAINTS.OWNER,ALL_CONSTRAINTS.CONSTRAINT_NAME
)PKS ON 
PKS.CONSTRAINT_NAME=ALL_CONSTRAINTS.R_CONSTRAINT_NAME
WHERE CONSTRAINT_TYPE='R'
GROUP BY
ALL_CONSTRAINTS.TABLE_NAME,ALL_CONSTRAINTS.OWNER,ALL_CONSTRAINTS.CONSTRAINT_NAME,PKS.TABLE_NAME,PKS.PKS;


select UPPER('Create Sequence '||SEQUENCE_OWNER||'.'||sequence_name||
       ' increment by '||increment_by||
       ' start with '||'1'||
       ' maxvalue '||max_value||
       decode(cycle_flag,'N',' NOCYCLE ',' CYCLE ')||
       decode(cache_size,0,'NOCACHE ','CACHE '||cache_size))||';'
 from ALL_sequences WHERE SEQUENCE_OWNER='HANFN';
 
 

CREATE USER SCHBDW IDENTIFIED BY "123";
GRANT CREATE TABLE TO SCHBDW;
GRANT CREATE SESSION TO SCHBDW;
GRANT UNLIMITED TABLESPACE TO SCHBDW;


SELECT * FROM V$TRANSACTION;

CREATE OR REPLACE Function convert_long_to_char( pTableName in varchar2,
                                                     pColumnName in varchar2,
                                                     pRowId in rowid ) return varchar2 As
      vCursor    integer default dbms_sql.open_cursor;
      vN         number;
      vLongVal  varchar2(4000);
      vLongLen  number;
      vBuflen    number := 4000;
      vCurPos    number := 0;
   begin
     dbms_sql.parse( vCursor,'select ' || pColumnName || ' from ' || pTableName ||
                                                             ' where rowid = :x',
                     dbms_sql.native );
     dbms_sql.bind_variable( vCursor, ':x', pRowId );
   
     dbms_sql.define_column_long(vCursor, 1);
     vN := dbms_sql.execute(vCursor);
   
     if (dbms_sql.fetch_rows(vCursor)>0) then
       dbms_sql.column_value_long(vCursor, 1, vBuflen, vCurpos ,
                                  vLongVal, vLongLen );
     end if;
     dbms_sql.close_cursor(vCursor);
     return vLongVal;
   end convert_long_to_char;
   /
   

   
exec run_query(q'#SELECT UNQ_ID_IN_SRC_STM_ID||'~'||GEN_RAND.GEN_VARCHAR_ALT2(8,'0123456789ABCDEF') A FROM HANFN.IP@DLKHIST#','LEGO_ACT','output.txt');

CREATE OR REPLACE PROCEDURE run_query(p_sql IN VARCHAR2
                                     ,p_dir IN VARCHAR2
                                     ,p_header_file IN VARCHAR2
                                     ,p_data_file IN VARCHAR2 := NULL) IS
  v_finaltxt  VARCHAR2(4000);
  v_v_val     VARCHAR2(4000);
  v_n_val     NUMBER;
  v_d_val     DATE;
  v_ret       NUMBER;
  c           NUMBER;
  d           NUMBER;
  col_cnt     INTEGER;
  f           BOOLEAN;
  rec_tab     DBMS_SQL.DESC_TAB;
  col_num     NUMBER;
  v_fh        UTL_FILE.FILE_TYPE;
  v_samefile  BOOLEAN := (NVL(p_data_file,p_header_file) = p_header_file);
BEGIN
  c := DBMS_SQL.OPEN_CURSOR;
  DBMS_SQL.PARSE(c, p_sql, DBMS_SQL.NATIVE);
  d := DBMS_SQL.EXECUTE(c);
  DBMS_SQL.DESCRIBE_COLUMNS(c, col_cnt, rec_tab);
  FOR j in 1..col_cnt
  LOOP
    CASE rec_tab(j).col_type
      WHEN 1 THEN DBMS_SQL.DEFINE_COLUMN(c,j,v_v_val,2000);
      WHEN 2 THEN DBMS_SQL.DEFINE_COLUMN(c,j,v_n_val);
      WHEN 12 THEN DBMS_SQL.DEFINE_COLUMN(c,j,v_d_val);
    ELSE
      DBMS_SQL.DEFINE_COLUMN(c,j,v_v_val,2000);
    END CASE;
  END LOOP;
  -- This part outputs the HEADER
  v_fh := UTL_FILE.FOPEN(upper(p_dir),p_header_file,'w',32767);
  FOR j in 1..col_cnt
  LOOP
    v_finaltxt := ltrim(v_finaltxt||','||lower(rec_tab(j).col_name),',');
  END LOOP;
  --  DBMS_OUTPUT.PUT_LINE(v_finaltxt);
  UTL_FILE.PUT_LINE(v_fh, v_finaltxt);
  IF NOT v_samefile THEN
    UTL_FILE.FCLOSE(v_fh);
  END IF;
  --
  -- This part outputs the DATA
  IF NOT v_samefile THEN
    v_fh := UTL_FILE.FOPEN(upper(p_dir),p_data_file,'w',32767);
  END IF;
  LOOP
    v_ret := DBMS_SQL.FETCH_ROWS(c);
    EXIT WHEN v_ret = 0;
    v_finaltxt := NULL;
    FOR j in 1..col_cnt
    LOOP
      CASE rec_tab(j).col_type
        WHEN 1 THEN DBMS_SQL.COLUMN_VALUE(c,j,v_v_val);
                    v_finaltxt := ltrim(v_finaltxt||',"'||v_v_val||'"',',');
        WHEN 2 THEN DBMS_SQL.COLUMN_VALUE(c,j,v_n_val);
                    v_finaltxt := ltrim(v_finaltxt||','||v_n_val,',');
        WHEN 12 THEN DBMS_SQL.COLUMN_VALUE(c,j,v_d_val);
                    v_finaltxt := ltrim(v_finaltxt||','||to_char(v_d_val,'DD/MM/YYYY HH24:MI:SS'),',');
      ELSE
        v_finaltxt := ltrim(v_finaltxt||',"'||v_v_val||'"',',');
      END CASE;
    END LOOP;
  --  DBMS_OUTPUT.PUT_LINE(v_finaltxt);
    UTL_FILE.PUT_LINE(v_fh, v_finaltxt);
  END LOOP;
  UTL_FILE.FCLOSE(v_fh);
  DBMS_SQL.CLOSE_CURSOR(c);
END;
/

CREATE OR REPLACE DIRECTORY LEGO_ACT AS 'D:\Asignaciones\Listas\LEGO_AyM PMO22297\Entrega\Actualización Número de Documento Externo'
/
GRANT READ, WRITE ON DIRECTORY LEGO_ACT TO JFVARGAS
/ 

