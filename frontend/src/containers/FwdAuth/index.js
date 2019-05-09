import React, { useEffect } from "react";
import { Redirect } from "react-router-dom";
import { useDispatch, useSelector } from "react-redux";
import { parse } from "query-string";
import { FORWARD_CODE } from "../../ducks/auth";
import Spinner from "../../components/Spinner";

export function FwdAuth({ location: { search } }) {
  const { code, state } = parse(search);

  const { done, error } = useSelector(({ auth: { done, error } }) => ({
    done,
    error
  }));

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch({ type: FORWARD_CODE, code, state });
  }, []);

  return done || error ? <Redirect to="/" /> : <Spinner />;
}

export default FwdAuth;
