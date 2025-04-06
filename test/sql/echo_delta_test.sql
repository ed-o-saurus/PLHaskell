DO
$$
DECLARE
    delta_comp delta;
BEGIN
    delta_comp := (((('abc', 42, 42.3), 0), '()'::charlie))::delta;

    IF echo(delta_comp) <> delta_comp THEN
        raise EXCEPTION 'echo delta failed';
    END IF;
END
$$;
