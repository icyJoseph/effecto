import React from "react";
import { Link } from "react-router-dom";
import { slide as Menu } from "react-burger-menu";
import { decorator } from "redux-burger-menu";
import "./index.css";

export const BurgerMenu = props => {
  return (
    <Menu>
      <Link className="menu-item" to="/">
        Home
      </Link>
      <Link className="menu-item" to="/create">
        Create
      </Link>
      <Link className="menu-item" to="/meetings">
        Meetings
      </Link>
      <Link className="menu-item" to="/notes">
        Notes
      </Link>
      <Link className="menu-item" to="/contacts">
        Contacts
      </Link>
      <Link className="menu-item" to="/stats">
        Stats
      </Link>
    </Menu>
  );
};

export default decorator(BurgerMenu);
