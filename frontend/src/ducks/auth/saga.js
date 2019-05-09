import { all, call, put, takeLatest, select } from "redux-saga/effects";
import { forwardCode, getProfile } from "./api";
import {
  FORWARD_CODE,
  SET_TOKEN,
  SET_PROFILE,
  GET_PROFILE,
  AUTH_ERROR
} from "./";
import { normalize } from "./helpers";

export function* tokenSaga({ code, state }) {
  try {
    const { access_token: accessToken, expires_in: expiry } = yield call(
      forwardCode,
      code,
      state
    );
    yield put({ type: SET_TOKEN, accessToken, expiry });
    yield put({ type: GET_PROFILE });
  } catch (e) {
    yield put({ type: AUTH_ERROR });
  }
}

export function* profileSaga() {
  try {
    const token = yield select(({ auth: { accessToken } }) => accessToken);
    const profile = yield call(getProfile, token);
    const normalized = normalize(profile);
    yield put({ type: SET_PROFILE, ...normalized });
  } catch (e) {
    yield put({ type: AUTH_ERROR });
  }
}

export default function* authSaga() {
  yield all([
    takeLatest(FORWARD_CODE, tokenSaga),
    takeLatest(GET_PROFILE, profileSaga)
  ]);
}
