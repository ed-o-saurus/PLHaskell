DO
$$
BEGIN
    IF query_array_select() != mk_array(6) THEN
        raise EXCEPTION 'array query failed';
    END IF;
END
$$;
