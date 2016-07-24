DROP FUNCTION STANDARIZE_COLUMNS;
CREATE OR REPLACE FUNCTION STANDARIZE_COLUMNS(STR VARCHAR2)
RETURN VARCHAR2
IS
BEGIN
RETURN
REPLACE(
q'#SELECT 
UPPER(utl_raw.cast_to_varchar2((nlssort(TRIM(COLUMN_VALUE), 'nls_sort=binary_ai')))) text
FROM 
(
SELECT 
'<str>' text
FROM DUAL
),xmltable(('"'|| REPLACE(text, chr(10), '","')|| '"'))#','<str>',STR);
END;
/

drop type dyn_col;
create type dyn_col as object
   (
     atype anytype,
 
     static function ODCITableDescribe(rtype out anytype,
                                       stmt  in  varchar2)
       return number,
 
     static function ODCITablePrepare(sctx      out dyn_col,
                                      tf_info   in  sys.ODCITabfuncinfo,
                                      stmt      in  varchar2)
       return number,
 
     static function ODCITableStart(sctx  in out dyn_col,
                                    stmt  in     varchar2)
       return number,
 
     member function ODCITablefetch(self  in out dyn_col,
                                    nrows in     number,
                                   rws   out    anydataset)
       return number,
 
     member function ODCITableClose(self in dyn_col)
       return number
   );
   /

drop package pkg_col;  
create package pkg_col
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
       return anydataset pipelined using dyn_col;
   
 
   end pkg_col;
   /
   
   create type body dyn_col
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
      r_sql   pkg_col.dynamic_sql_rec;
      -- Type to create (has all the columns) of the sql query.
      t_anyt  anytype;
      -- SQL query that will be made up from the 2 passed in queries.
      v_sql   varchar2(32767);

    begin

      /*
       * Parse the SQL and describe its format and structure.
       */
      v_sql := replace(STANDARIZE_COLUMNS(stmt), ';', null);

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
    static function ODCITableprepare(sctx      out dyn_col,
                                     tf_info   in  sys.ODCITabfuncinfo,
                                     stmt      in  varchar2)
      return number
    is

      /* Variables */
      -- Meta data.
      r_meta   pkg_col.anytype_metadata_rec;

    begin

      r_meta.typecode := tf_info.rettype.getattreleminfo(
                           1, r_meta.precision, r_meta.scale, r_meta.length,
                           r_meta.csid, r_meta.csfrm, r_meta.type, r_meta.name
                         );

      sctx := dyn_col(r_meta.type);
      return odciconst.success;

    end;


    /*
     * START step. this is where we execute the cursor prior to fetching from it.
     */
    static function ODCITablestart(sctx  in out dyn_col,
                                   stmt  in     varchar2)
      return number
    is

      /* Variables */
      r_meta pkg_col.anytype_metadata_rec;
      v_sql varchar2(32767);
    begin

      v_sql := replace(STANDARIZE_COLUMNS(stmt), ';', null);
      pkg_col.r_sql.cursor := dbms_sql.open_cursor;
      dbms_sql.parse(pkg_col.r_sql.cursor, v_sql, dbms_sql.native);
      dbms_sql.describe_columns2(pkg_col.r_sql.cursor,
                                 pkg_col.r_sql.column_cnt,
                                 pkg_col.r_sql.description);

      -- define all the columns found to let Oracle know the datatypes.
      for i in 1..pkg_col.r_sql.column_cnt
      loop

        r_meta.typecode := sctx.atype.GetAttrElemInfo(
                             i, r_meta.precision, r_meta.scale, r_meta.length,
                             r_meta.csid, r_meta.csfrm, r_meta.type, r_meta.name
                           );

        case r_meta.typecode
          when dbms_types.typecode_varchar2
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, '', 32767);
          when dbms_types.typecode_number
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as number));
          when dbms_types.typecode_date
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as date));
          when dbms_types.typecode_raw
          then
            dbms_sql.define_column_raw(pkg_col.r_sql.cursor, i, cast(null as raw), r_meta.length);
          when dbms_types.typecode_timestamp
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as timestamp));
          when dbms_types.typecode_timestamp_tz
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as timestamp with time zone));
          when dbms_types.typecode_timestamp_ltz
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as timestamp with local time zone));
          when dbms_types.typecode_interval_ym
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as interval year to month));
          when dbms_types.typecode_interval_ds
          then
            dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as interval day to second));
          when dbms_types.typecode_clob
          then
            case pkg_col.r_sql.description(i).col_type
              when 8
              then
                dbms_sql.define_column_long(pkg_col.r_sql.cursor, i);
              else
                dbms_sql.define_column(pkg_col.r_sql.cursor, i, cast(null as clob));
            end case;
        end case;
      end loop;

      -- execute the SQL.
      pkg_col.r_sql.execute := dbms_sql.execute(pkg_col.r_sql.cursor);

      return odciconst.success;

    end ODCITablestart;


    /*
     * FETCH step.
     */
    member function ODCITablefetch(self   in out dyn_col,
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
      r_meta  pkg_col.anytype_metadata_rec;

    begin

      if dbms_sql.fetch_rows( pkg_col.r_sql.cursor ) > 0
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
        for i in 1..pkg_col.r_sql.column_cnt
        loop

          r_meta.typecode := self.atype.getattreleminfo(
                               i, r_meta.precision, r_meta.scale, r_meta.length,
                               r_meta.csid, r_meta.csfrm, r_meta.attr_type,
                               r_meta.attr_name
                             );

          case r_meta.typecode
            when dbms_types.typecode_varchar2
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_vc_col);
              rws.setvarchar2(v_vc_col);
            when dbms_types.typecode_number
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_num_col);
              rws.setnumber(v_num_col);
            when dbms_types.typecode_date
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_date_col);
              rws.setdate(v_date_col);
            when dbms_types.typecode_raw
            then
              dbms_sql.column_value_raw(pkg_col.r_sql.cursor, i, v_raw_col,
                 v_raw_error, v_raw_len);
              rws.setraw(v_raw_col);
            when dbms_types.typecode_interval_ds
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_int_ds_col);
              rws.setintervalds(v_int_ds_col);
            when dbms_types.typecode_interval_ym
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_int_ym_col);
              rws.setintervalym(v_int_ym_col);
            when dbms_types.typecode_timestamp
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_ts_col);
              rws.settimestamp(v_ts_col);
            when dbms_types.typecode_timestamp_tz
            then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_tstz_col);
              rws.settimestamptz(v_tstz_col);
           when dbms_types.typecode_timestamp_ltz
           then
              dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_tsltz_col);
              rws.settimestampltz(v_tsltz_col);
           when dbms_types.typecode_clob
           then
             case pkg_col.r_sql.description(i).col_type
               when 8
               then
                 loop
                   dbms_sql.column_value_long(pkg_col.r_sql.cursor, i, 32767, v_clob_offset,
                                              v_vc_col, v_clob_len);
                   v_clob_col := v_clob_col || v_vc_col;
                   v_clob_offset := v_clob_offset + 32767;
                   exit when v_clob_len < 32767;
                 end loop;
               else
                 dbms_sql.column_value(pkg_col.r_sql.cursor, i, v_clob_col);
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
    member function ODCITableClose(self in dyn_col)
      return number
    is


    begin
      dbms_sql.close_cursor( pkg_col.r_sql.cursor );
      pkg_col.r_sql := null;
      return odciconst.success;
    end ODCITableClose;

  end;
  /


/*
select * from table(pkg_col.querydb(
'a
b
c
d'));


select 
MULTIPLE_REPLACE(q'#FIELD(TRAMA,',',<n>) <f>,#',NEW T_TEXT('<n>','<f>'),NEW T_TEXT(ROWNUM,trim(TEXT))),
TEXT
from 
table(pkg_col.querydb(
'TipoRegistro
TipoIdentificacion
NumIdenticacion
CodOficina
TipoProducto
CodProducto
TipoCuenta
TipoMoneda
SaldoCapitalCorte
SaldoInteresCte
EstatusPropiedad
DígitoChequeo
Tipodecliente
Tipodepersonajurídica
Nombreorazónsocial
Celular
Correoelectrónico
Códigodeldepartamento
Códigodelmunicipio'));
*/