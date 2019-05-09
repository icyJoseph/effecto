import { applyMiddleware, createStore, compose } from "redux";
import createSagaMiddleware from "redux-saga";
import rootSaga from "../ducks/rootSaga";
import rootReducer from "../ducks/rootReducer";
import { persistAuth } from "./middleware";

const composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__
  ? window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__({})
  : compose;

const sagaMiddleware = createSagaMiddleware();
const enhancer = composeEnhancers(applyMiddleware(sagaMiddleware, persistAuth));

export default function(initialState) {
  const store = createStore(rootReducer, initialState, enhancer);
  sagaMiddleware.run(rootSaga);

  if (module.hot) {
    module.hot.accept("../ducks/rootReducer", () => {
      const nextRootReducer = require("../ducks/rootReducer");
      store.replaceReducer(nextRootReducer);
    });
  }
  return store;
}
