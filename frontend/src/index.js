import React from "react";
import ReactDOM from "react-dom";
import { Provider } from "react-redux";
import { ThemeProvider } from "styled-components";
import configureStore from "./store";
import Routes from "./routes";
import theme from "./styles/theme";
import { KEY } from "./store/middleware";
import * as serviceWorker from "./serviceWorker";

const savedAuth = localStorage.getItem(KEY);
const initialState = savedAuth ? { auth: JSON.parse(savedAuth) } : undefined;
const store = configureStore(initialState);

ReactDOM.render(
  <Provider store={store}>
    <ThemeProvider theme={theme}>
      <Routes />
    </ThemeProvider>
  </Provider>,
  document.getElementById("root")
);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
