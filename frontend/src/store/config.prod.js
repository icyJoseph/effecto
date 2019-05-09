import { applyMiddleware, createStore, compose } from "redux";
import createSagaMiddleware from "redux-saga";
import rootSaga from "../ducks/rootSaga";
import rootReducer from "../ducks/rootReducer";
import { persistAuth } from "./middleware";

const sagaMiddleware = createSagaMiddleware();
const middlewares = applyMiddleware(sagaMiddleware, persistAuth);

const enhancer = compose(middlewares);

export default function(initialState) {
  const store = createStore(rootReducer, initialState, enhancer);
  sagaMiddleware.run(rootSaga);
  return store;
}
