DO
$$
BEGIN
    IF delta_func() <> (((('abc', 42, 42.3), 0), '()'::charlie))::delta THEN
        raise EXCEPTION 'delta_test failed';
    END IF;
END
$$;
