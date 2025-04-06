DO $$
DECLARE
    alpha_array alpha[];
BEGIN
    SELECT array_agg(alpha_func(i))
    INTO alpha_array
    FROM generate_series(1, 4) i;

    IF alpha_array != echo(alpha_array) THEN
        raise EXCEPTION 'alpha_array failed';
    END IF;
END
$$;
